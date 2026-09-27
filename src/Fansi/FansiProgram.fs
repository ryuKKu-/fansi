namespace Fansi

open Elmish
open System
open Fansi
open Fansi.Core
open Fansi.Renderer
open System.Runtime.ExceptionServices
open System.Runtime.InteropServices
open System.Text
open System.Threading
open System.Threading.Tasks

type private InternalProgram<'model, 'msg>
    (renderer: Renderer, program: Program<unit, 'model, FansiMsg<'msg>, Node>, mouseEnabled: bool, quitOnCtrlC: bool) =
    let mutable dispatch = ignore<FansiMsg<'msg>>
    let mutable oldModel: 'model option = None
    let mutable viewTree = Ui.empty
    let mutable p = Unchecked.defaultof<Program<unit, 'model, FansiMsg<'msg>, Node>>
    let mutable runProgramLoop = fun () -> ()
    let mutable stopProgram = fun () -> ()

    // Written by whichever thread terminates the program, read by the reader.
    [<VolatileField>]
    let mutable quit = false

    // Written by the reader thread, read by Run once the quit signal arrives.
    [<VolatileField>]
    let mutable readerError: exn option = None

    // What Run waits on. A read of stdin cannot be cancelled, so anything waiting
    // on the reader cannot be woken by a quit that came from somewhere else.
    // Continuations run off this thread so the terminate path, which holds the
    // pump lock, never ends up running the shutdown itself.
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

    let escapeTimeout = 50

    // A tail that is not a paste is given up on after three escape timeouts, 150
    // ms: far longer than a terminal takes to send the rest of a sequence, far
    // shorter than a person notices.
    let shortTailTimeouts = 3

    // A paste gets two seconds of silence before its text is handed over without
    // its framing. Long enough that a slow link never chops one in half.
    let stalledPasteTimeouts = 40

    // How much of a paste the reader holds before handing over a chunk of it. Big
    // enough that a pasted file, log or certificate still arrives as one Paste,
    // small enough to bound the copying the growable buffer costs on every read.
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

        // Raw mode clears the signal the terminal would normally raise, so Ctrl+C
        // is just a byte now. The app still sees the key; unless it asked to keep
        // the key for itself, the key also leaves.
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

    /// What a buffer the reader is giving up on still owes the app. A paste hands
    /// over what it has, because losing the framing beats losing what was typed. A
    /// malformed sequence owes nothing: keeping it would glue every later keystroke
    /// onto the bad prefix and misread all of them.
    let stuckPasteText insidePaste (tail: byte array) =
        let bodyAt = if insidePaste then 0 else pasteBodyAt tail

        if bodyAt >= 0 && bodyAt < tail.Length then
            Some(Encoding.UTF8.GetString(tail, bodyAt, tail.Length - bodyAt))
        else
            None

    let readInput () =
        task {
            let stdin = Console.OpenStandardInput()
            let chunk = Array.zeroCreate<byte> 1024
            // Whatever the parser could not finish. Carrying it forward is what
            // makes a sequence or a paste split across reads arrive whole.
            let mutable pending = Array.empty<byte>
            // A read that lost the escape-timeout race is still running against
            // chunk. Keep waiting on that same task next time round rather than
            // starting a second concurrent read on the same stream and buffer.
            let mutable outstandingRead: Task<int> option = None
            let mutable stuckTimeouts = 0
            // Set once part of a paste has gone to the app. Its start marker went
            // with those bytes, so from here the reader watches for the terminator
            // itself rather than letting the parser read the rest as keystrokes.
            let mutable inPaste = false

            while not quit do
                let read =
                    match outstandingRead with
                    | Some r -> r
                    | None -> stdin.ReadAsync(chunk, 0, chunk.Length)

                let! finished =
                    // Mid-paste the buffer can be empty and still be waiting on
                    // something, so the timeout has to run there too.
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
                                // The delay lost. Cancelling drops its timer entry,
                                // which a long paste would otherwise leak per read.
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

                        // A paste still coming in gets real time before its text is
                        // handed over without its framing. A sequence that will
                        // never finish goes quickly.
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
                    // End of a pipe. Nothing more is coming, so the tail gets its
                    // last chance instead of going down with the stream.
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
                        // The terminator can straddle this boundary, so keep back
                        // enough bytes to still recognise it on the next read.
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
                    readerError <- Some ex
            finally
                // However the reader ends, Run has to stop waiting for it.
                stopped.TrySetResult() |> ignore

        let thread = Thread(ThreadStart body)
        // Nothing can cancel a read of stdin, so the reader may still be parked in
        // one when the program quits. A background thread does not hold the process
        // open, which is what makes quitting from anywhere else work.
        //
        // The trap for a caller: that parked read is still holding stdin after run
        // returns, so the first thing typed afterwards can go to it instead of to
        // the caller. A program that reads stdin again after run should exit
        // instead.
        thread.IsBackground <- true
        thread.Start()

    /// SIGWINCH where it exists, polling where it does not. Either way the size is
    /// compared before dispatching, so a repaint only happens on a real change.
    let watchResize () =
        let mutable lastW = 0
        let mutable lastH = 0

        let check () =
            try
                let w = Console.WindowWidth
                let h = Console.WindowHeight

                if w <> lastW || h <> lastH then
                    lastW <- w
                    lastH <- h
                    dispatch (Resize(w, h))
            with _ ->
                // No console attached. Nothing to watch.
                ()

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

            p <-
                program
                |> Program.withTermination (fun _ -> quitToken.Requested) (fun _ -> ())
                |> Program.map
                    (fun init arg ->
                        let model, cmd = init arg
                        model, setDispatch :: cmd)
                    id
                    id
                    (fun _ model dispatch -> setState model dispatch)
                    id
                    (fun (predicate, terminate) ->
                        predicate,
                        (fun m ->
                            terminate m
                            quit <- true
                            stopped.TrySetResult() |> ignore))

            let start, stop = Program'.runFirstRender () p
            runProgramLoop <- start
            stopProgram <- stop

            setState <-
                fun model dispatch ->
                    match oldModel with
                    | Some old when shouldRender old model ->
                        viewTree <- Program.view p model dispatch
                        oldModel <- Some model
                        renderView viewTree
                    | None ->
                        viewTree <- Program.view p model dispatch
                        oldModel <- Some model
                        renderView viewTree
                    | _ -> ()

            try
                runProgramLoop ()

                // Paints the first frame. runFirstRender calls setState while it is
                // still the non-rendering version assigned at init, and the rendering
                // version installed above only runs when a message arrives — so an app
                // whose init dispatches nothing would never paint without this.
                renderView viewTree

                use _resize = watchResize ()
                // The reader runs on its own thread and this waits on the quit
                // signal. Waiting on the reader instead would hang every quit that
                // did not come from a keystroke.
                startReader ()
                do! stopped.Task
            finally
                renderer.Stop()

            match readerError with
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
              quitOnCtrlC: bool }

    let mkProgram
        (init: unit -> 'model * Cmd<'msg>)
        (update: FansiMsg<'msg> -> 'model -> 'model * Cmd<'msg>)
        (view: 'model -> Node)
        =
        // The app returns plain Cmd<'msg>; the framework lifts it back into
        // FansiMsg so a child component's commands need only Cmd.map.
        let lift (model, cmd) = model, Cmd.map App cmd

        { program =
            Program.mkProgram (fun () -> lift (init ())) (fun msg model -> lift (update msg model)) (fun model _ ->
                view model)
          fps = 60<FPS>
          mouseEnabled = false
          quitOnCtrlC = true }

    let withFps fps (p: FansiProgram<'model, 'msg>) = { p with fps = fps }

    let withMouseEnabled (p: FansiProgram<'model, 'msg>) = { p with mouseEnabled = true }

    /// Keeps Ctrl+C for the app. It still arrives as a key event either way; this
    /// only stops the program quitting on it, so an app that takes the key also
    /// takes the job of leaving.
    let withoutQuitOnCtrlC (p: FansiProgram<'model, 'msg>) = { p with quitOnCtrlC = false }

    let withSubscription subscribe (p: FansiProgram<'model, 'msg>) =
        { p with
            program = Program.withSubscription subscribe p.program }

    let run (p: FansiProgram<'model, 'msg>) =
        let renderer = Renderer(p.fps)
        use _raw = Terminal.enterRawMode ()

        renderer.Execute(AnsiSequence.enableAltScreenBuffer)
        renderer.Execute(AnsiSequence.hideCursor)
        renderer.Execute(AnsiSequence.enableBracketedPaste)
        renderer.Execute(AnsiSequence.enableFocusReporting)

        if p.mouseEnabled then
            renderer.Execute(AnsiSequence.enableMouseTracking)

        renderer.Start()

        let program = InternalProgram(renderer, p.program, p.mouseEnabled, p.quitOnCtrlC)

        try
            do program.Run() |> Async.AwaitTask |> Async.RunSynchronously
        finally
            if p.mouseEnabled then
                renderer.Execute(AnsiSequence.disableMouseTracking)

            renderer.Execute(AnsiSequence.disableFocusReporting)
            renderer.Execute(AnsiSequence.disableBracketedPaste)
            renderer.Execute(AnsiSequence.showCursor)
            renderer.Execute(AnsiSequence.disableAltScreenBuffer)
