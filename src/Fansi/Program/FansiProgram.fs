namespace Fansi

open Elmish
open System
open System.IO
open Fansi
open Fansi.Core
open Fansi.Renderer
open Microsoft.Win32.SafeHandles
open System.Runtime.ExceptionServices
open System.Runtime.InteropServices
open System.Text
open System.Threading
open System.Threading.Tasks

type private InternalProgram<'model, 'msg>
    (renderer: Renderer, program: Program<unit, 'model, FansiMsg<'msg>, Node>, quitOnCtrlC: bool, escapeTimeout: int) =
    let mutable dispatch = ignore<FansiMsg<'msg>>
    let mutable oldModel: 'model option = None
    let mutable viewTree = Ui.empty
    let mutable p = Unchecked.defaultof<Program<unit, 'model, FansiMsg<'msg>, Node>>
    let mutable runProgramLoop = fun () -> ()
    let mutable stopProgram = fun () -> ()

    // Written by whichever thread terminates the program, read by the reader.
    [<VolatileField>]
    let mutable quit = false

    // The first exception that ended the program, from the reader or from update
    // on whichever thread. Run rethrows it once the quit signal arrives.
    let mutable failure: exn option = None
    let failureGate = obj ()

    // Set when a Resize is dispatched, cleared by the repaint it forces. An app
    // whose update returns the same model for a Resize still needs a new frame at
    // the new size.
    let mutable resized = false

    // What Run waits on. A read of stdin cannot be cancelled. A quit from
    // another source therefore cannot wake anything that waits on the reader.
    // Continuations run on another thread. The terminate path holds the pump
    // lock, so it never runs the shutdown itself.
    let stopped =
        TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)

    let termSize () =
        // WindowWidth throws when no console is attached. A piped or headless run
        // still has to paint something.
        try
            max 1 Console.WindowWidth, max 1 Console.WindowHeight
        with _ ->
            80, 24

    let renderView (tree: Node) =
        let width, height = termSize ()
        renderer.SetFrame(Paint.render width height tree)

    let mutable setState =
        fun model dispatch ->
            viewTree <- Program.view p model dispatch
            oldModel <- Some model

    let equal = fun m1 m2 -> obj.ReferenceEquals(m1, m2)

    let shouldRender oldModel newModel = not <| equal oldModel newModel

    // The reader stops waiting for a tail that is not a paste after three
    // escape timeouts, which is 150 ms by default. This is far longer than a
    // terminal needs to send the rest of a sequence. It is far shorter than a
    // person can notice.
    let shortTailTimeouts = 3

    // A paste waits for two seconds of silence before the reader passes on its
    // text without the framing. This is long enough that a slow link never
    // splits a paste in half. It counts time, not timeouts, so a longer escape
    // timeout does not extend it.
    let stalledPasteTimeouts = max 1 (2000 / escapeTimeout)

    // How much of a paste the reader holds before it passes on a chunk. The
    // limit is big enough that a pasted file, log or certificate still arrives
    // as one Paste. It is small enough to limit the copying that the growable
    // buffer costs on every read.
    let maxPending = 1024 * 1024

    let pasteStartBytes = Encoding.ASCII.GetBytes InputParser.PasteStart
    let pasteEndBytes = Encoding.ASCII.GetBytes InputParser.PasteEnd

    let toMsg event =
        match event with
        | InputEvent.Key k -> KeyPress k
        | InputEvent.Mouse m -> Mouse m
        | InputEvent.Paste p -> Paste p
        | InputEvent.Resize(w, h) -> Resize(w, h)
        | InputEvent.FocusChanged f -> FocusChanged f

    let isCtrlC (k: KeyEvent) =
        k.Ctrl && not k.Alt && k.Key = Key.Char 'c'

    let dispatchEvent event =
        dispatch (toMsg event)

        // Raw mode removes the signal that the terminal normally raises, so
        // Ctrl+C is only a byte. The app still sees the key. Unless the app
        // asked to keep the key, the key also ends the program.
        match event with
        | InputEvent.Key k when quitOnCtrlC && isCtrlC k -> stopProgram ()
        | _ -> ()

    /// Where a bracketed paste's text starts in this buffer, or -1 if the buffer
    /// does not open one.
    let pasteBodyAt (buffer: byte array) =
        let markerAt = ReadOnlySpan(buffer).IndexOf(ReadOnlySpan pasteStartBytes)

        if markerAt < 0 then
            -1
        else
            markerAt + pasteStartBytes.Length

    /// Where a bracketed paste's text ends in this buffer, or -1.
    let pasteEndAt (buffer: byte array) =
        ReadOnlySpan(buffer).IndexOf(ReadOnlySpan pasteEndBytes)

    /// What the reader still sends to the app from a buffer that it abandons. A
    /// paste passes on what it has, because losing the framing is better than
    /// losing what the user typed. A malformed sequence gives nothing. If kept,
    /// it would attach every later keystroke to the bad prefix, and the parser
    /// would misread all of them.
    let stuckPasteText insidePaste (tail: byte array) =
        let bodyAt = if insidePaste then 0 else pasteBodyAt tail

        if bodyAt >= 0 && bodyAt < tail.Length then
            Some(Encoding.UTF8.GetString(tail, bodyAt, tail.Length - bodyAt))
        else
            None

    /// On Unix, Console.OpenStandardInput on a terminal uses the line editor of
    /// .NET. It echoes, waits for Enter and rewrites termios around every read.
    /// Reading fd 0 directly returns the bytes as the user types them. Windows
    /// in raw mode with virtual-terminal input already does this.
    let openInput () : Stream =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Console.OpenStandardInput()
        else
            // bufferSize 0 means no extra buffering on top of the reads.
            new FileStream(new SafeFileHandle(0n, false), FileAccess.Read, 0)

    let recordFailure ex =
        lock failureGate (fun () ->
            if failure.IsNone then
                failure <- Some ex)

    let readInput () =
        task {
            let stdin = openInput ()
            let chunk = Array.zeroCreate<byte> (64 * 1024)
            // Whatever the parser could not finish. Keeping it for the next
            // read makes a sequence or a paste that is split across reads
            // arrive whole.
            let mutable pending = Array.empty<byte>
            // A read that lost the escape-timeout race is still running against
            // chunk. On the next loop, keep waiting on that same task. Do not
            // start a second concurrent read on the same stream and buffer.
            let mutable outstandingRead: Task<int> option = None
            let mutable stuckTimeouts = 0
            // Set when part of a paste has gone to the app. Its start marker
            // went with those bytes. From here, the reader watches for the
            // terminator itself. The parser does not read the rest as
            // keystrokes.
            let mutable inPaste = false

            while not quit do
                let read =
                    match outstandingRead with
                    | Some r -> r
                    | None -> stdin.ReadAsync(chunk, 0, chunk.Length)

                let! finished =
                    // In the middle of a paste, the buffer can be empty and
                    // still wait for something. The timeout must therefore run
                    // there too.
                    if pending.Length = 0 && not inPaste then
                        task {
                            let! n = read
                            return Some n
                        }
                    else
                        task {
                            use timeout = new CancellationTokenSource()
                            let! winner = Task.WhenAny(read, Task.Delay(escapeTimeout, timeout.Token))

                            if obj.ReferenceEquals(winner, read) then
                                // The delay lost. Cancelling removes its timer
                                // entry. Without this, a long paste would leak
                                // one entry per read.
                                timeout.Cancel()
                                let! n = read
                                return Some n
                            else
                                return None
                        }

                match finished with
                | None ->
                    outstandingRead <- Some read

                    let consumed =
                        if inPaste then
                            // These bytes are paste text, not a sequence to finish.
                            0
                        else
                            // Nothing more came. A lone ESC is now the Escape key.
                            let events, consumed = InputParser.parseFinal (ReadOnlySpan pending)
                            events |> List.iter dispatchEvent
                            consumed

                    if consumed > 0 then
                        pending <- pending[consumed..]
                        stuckTimeouts <- 0
                    else
                        stuckTimeouts <- stuckTimeouts + 1

                        // A paste that is still arriving gets real time before
                        // the reader passes on its text without the framing. A
                        // sequence that will never finish is abandoned quickly.
                        let limit =
                            if inPaste || pasteBodyAt pending >= 0 then
                                stalledPasteTimeouts
                            else
                                shortTailTimeouts

                        if stuckTimeouts >= limit then
                            stuckPasteText inPaste pending |> Option.iter (Paste >> dispatch)
                            inPaste <- false
                            pending <- Array.empty
                            stuckTimeouts <- 0
                | Some 0 ->
                    // End of a pipe. Nothing more arrives, so the tail gets its
                    // last chance. It does not disappear with the stream.
                    if pending.Length > 0 then
                        if not inPaste then
                            let events, consumed = InputParser.parseFinal (ReadOnlySpan pending)
                            events |> List.iter dispatchEvent
                            pending <- pending[consumed..]

                        stuckPasteText inPaste pending |> Option.iter (Paste >> dispatch)
                        pending <- Array.empty

                    stopProgram ()
                | Some n ->
                    outstandingRead <- None
                    stuckTimeouts <- 0
                    let mutable buffer = Array.append pending chunk[.. n - 1]

                    if inPaste then
                        let endAt = pasteEndAt buffer

                        if endAt >= 0 then
                            if endAt > 0 then
                                dispatch (Paste(Encoding.UTF8.GetString(buffer, 0, endAt)))

                            inPaste <- false
                            buffer <- buffer[endAt + pasteEndBytes.Length ..]

                    if inPaste then
                        pending <- buffer
                    else
                        let events, consumed = InputParser.parse (ReadOnlySpan buffer)
                        events |> List.iter dispatchEvent
                        pending <- buffer[consumed..]

                    if pending.Length >= maxPending then
                        // The terminator can span this boundary. Hold enough
                        // bytes to recognise it on the next read.
                        let keep = pasteEndBytes.Length - 1

                        match stuckPasteText inPaste pending[.. pending.Length - keep - 1] with
                        | Some text ->
                            dispatch (Paste text)
                            // The rest of the paste is still coming and its start
                            // marker has just gone out with this chunk.
                            inPaste <- true
                            pending <- pending[pending.Length - keep ..]
                        | None -> pending <- Array.empty
        }

    let startReader () =
        let body () =
            try
                try
                    (readInput ()).GetAwaiter().GetResult()
                with ex ->
                    recordFailure ex
                    // Subscriptions and timers would otherwise keep calling update
                    // after run has thrown.
                    stopProgram ()
            finally
                // However the reader ends, Run has to stop waiting for it.
                stopped.TrySetResult() |> ignore

        let thread = Thread(ThreadStart body)
        // Nothing can cancel a read of stdin, so the reader may still wait in
        // one when the program quits. A background thread does not keep the
        // process open. This makes quitting from any other source work.
        //
        // Warning for a caller: that waiting read still holds stdin after run
        // returns. The first input typed afterwards can go to it and not to the
        // caller. A program that reads stdin again after run should exit
        // instead.
        thread.IsBackground <- true
        thread.Start()

    /// Uses SIGWINCH where it exists and polling where it does not. In both
    /// cases the code compares the size before it dispatches. A repaint
    /// therefore happens only on a real change.
    let watchResize () =
        let mutable lastW = 0
        let mutable lastH = 0
        let gate = obj ()

        let readSize () =
            try
                Some(Console.WindowWidth, Console.WindowHeight)
            with _ ->
                // No console attached. Nothing to watch.
                None

        let check () =
            lock gate (fun () ->
                match readSize () with
                | Some(w, h) when w <> lastW || h <> lastH ->
                    lastW <- w
                    lastH <- h
                    dispatch (Resize(w, h))
                | _ -> ())

        // Starting from zero means the first check always dispatches. That is on
        // purpose: an app learns its size right after the first paint.
        check ()

        try
            let registration =
                PosixSignalRegistration.Create(PosixSignal.SIGWINCH, (fun _ -> check ()))

            registration :> IDisposable
        with _ ->
            let timer = new Timers.Timer(200.0)
            timer.Elapsed.Add(fun _ -> check ())
            timer.Start()

            { new IDisposable with
                member _.Dispose() =
                    timer.Stop()
                    timer.Dispose() }

    member this.Run() =
        task {
            let setDispatch d = dispatch <- d
            let quitToken = QuitToken.install ()

            // Runs inside the pump, just before update, so the flag and the repaint
            // it forces belong to the same message.
            let noteResize update msg model =
                match msg with
                | Resize _ -> resized <- true
                | _ -> ()

                update msg model

            p <-
                program
                |> Program.withTermination (fun _ -> quitToken.Requested) (fun _ -> ())
                |> Program.map
                    (fun init arg ->
                        let model, cmd = init arg
                        model, setDispatch :: cmd)
                    noteResize
                    id
                    (fun _ model dispatch -> setState model dispatch)
                    id
                    (fun (predicate, terminate) ->
                        predicate,
                        (fun m ->
                            terminate m
                            quit <- true
                            stopped.TrySetResult() |> ignore))

            setState <-
                fun model dispatch ->
                    let repaint = resized
                    resized <- false

                    match oldModel with
                    | Some old when repaint || shouldRender old model ->
                        viewTree <- Program.view p model dispatch
                        oldModel <- Some model
                        renderView viewTree
                    | None ->
                        viewTree <- Program.view p model dispatch
                        oldModel <- Some model
                        renderView viewTree
                    | _ -> ()

            // If update throws on any thread, the program stops. Run rethrows
            // the exception below, on the caller's thread.
            let onCrash ex =
                recordFailure ex
                stopProgram ()

            try
                let start, stop = Program'.runFirstRender onCrash () p
                runProgramLoop <- start
                stopProgram <- stop

                runProgramLoop ()

                // Paints the first frame. runFirstRender calls setState while
                // it is still the non-rendering version that init assigned. The
                // rendering version that the code installs above runs only when
                // a message arrives. Without this, an app whose init dispatches
                // nothing would never paint.
                renderView viewTree

                use _resize = watchResize ()
                // The reader runs on its own thread and this waits on the quit
                // signal. Waiting on the reader instead would hang every quit that
                // did not come from a keystroke.
                startReader ()
                do! stopped.Task
            finally
                renderer.Stop()

            match failure with
            | Some ex -> ExceptionDispatchInfo.Capture(ex).Throw()
            | None -> ()
        }

[<RequireQualifiedAccess>]
module FansiProgram =

    type FansiProgram<'model, 'msg> =
        private
            { program: Program<unit, 'model, FansiMsg<'msg>, Node>
              fps: int<FPS>
              mouseEnabled: bool
              quitOnCtrlC: bool
              escapeTimeout: int<ms> }

    let mkProgram
        (init: unit -> 'model * Cmd<'msg>)
        (update: FansiMsg<'msg> -> 'model -> 'model * Cmd<'msg>)
        (view: 'model -> Node)
        =
        // The app returns plain Cmd<'msg>. The framework lifts it into
        // FansiMsg, so the commands of a child component need only Cmd.map.
        let lift (model, cmd) = model, Cmd.map App cmd

        { program =
            Program.mkProgram (fun () -> lift (init ())) (fun msg model -> lift (update msg model)) (fun model _ ->
                view model)
          fps = 60<FPS>
          mouseEnabled = false
          quitOnCtrlC = true
          escapeTimeout = 50<ms> }

    let withFps fps (p: FansiProgram<'model, 'msg>) = { p with fps = fps }

    let withMouseEnabled (p: FansiProgram<'model, 'msg>) = { p with mouseEnabled = true }

    /// Keeps Ctrl+C for the app. It arrives as a key event in both cases. This
    /// option only stops the program from quitting on it. An app that takes the
    /// key also takes the job of exiting.
    let withoutQuitOnCtrlC (p: FansiProgram<'model, 'msg>) = { p with quitOnCtrlC = false }

    /// How long a lone Esc waits for the rest of a sequence before it counts as
    /// the Esc key. The default, 50 ms, suits a local terminal. Over a slow
    /// link such as SSH, arrow keys can arrive split and read as Esc plus
    /// letters. A longer timeout fixes that, but it makes Esc slower.
    let withEscapeTimeout (timeout: int<ms>) (p: FansiProgram<'model, 'msg>) =
        { p with
            escapeTimeout = max 1<ms> timeout }

    /// Subscriptions are started and disposed while the program holds its message
    /// lock. A Dispose that waits for the subscription's own dispatching thread to
    /// finish therefore deadlocks.
    let withSubscription (subscribe: 'model -> Sub<'msg>) (p: FansiProgram<'model, 'msg>) =
        { p with
            program = Program.withSubscription (subscribe >> Sub.map App) p.program }

    let run (p: FansiProgram<'model, 'msg>) =
        let renderer = Renderer(p.fps)

        let teardown =
            String.concat
                ""
                [ if p.mouseEnabled then
                      AnsiSequence.disableMouseTracking
                  AnsiSequence.disableFocusReporting
                  AnsiSequence.disableBracketedPaste
                  AnsiSequence.showCursor
                  AnsiSequence.disableAltScreenBuffer ]

        // Runs once on every exit, including a plain kill, and before the
        // program restores the terminal modes. If mouse tracking stays on, the
        // terminal types a report into the shell on every mouse move.
        let leave () =
            renderer.Stop()
            renderer.Execute teardown

        use _terminal = Terminal.enterRawModeWith leave

        renderer.Execute(AnsiSequence.enableAltScreenBuffer)
        renderer.Execute(AnsiSequence.hideCursor)
        renderer.Execute(AnsiSequence.enableBracketedPaste)
        renderer.Execute(AnsiSequence.enableFocusReporting)

        if p.mouseEnabled then
            renderer.Execute(AnsiSequence.enableMouseTracking)

        renderer.Start()

        let program =
            InternalProgram(renderer, p.program, p.quitOnCtrlC, int p.escapeTimeout)

        program.Run() |> Async.AwaitTask |> Async.RunSynchronously
