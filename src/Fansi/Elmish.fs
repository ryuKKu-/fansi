// AutoOpen so `open Fansi` alone reaches QuitToken and Cmd; everything else here
// is internal and stays out of a consumer's way regardless.
[<AutoOpen>]
module Fansi.Elmish

open Elmish

[<Struct>]
type internal RingState<'item> =
    | Writable of wx: 'item array * ix: int
    | ReadWritable of rw: 'item array * wix: int * rix: int

type internal RingBuffer<'item>(size) =
    let doubleSize ix (items: 'item array) =
        seq {
            yield! items |> Seq.skip ix
            yield! items |> Seq.take ix

            for _ in 0 .. items.Length do
                yield Unchecked.defaultof<'item>
        }
        |> Array.ofSeq

    let mutable state: 'item RingState = Writable(Array.zeroCreate (max size 10), 0)

    member _.Pop() =
        match state with
        | ReadWritable(items, wix, rix) ->
            let rix' = (rix + 1) % items.Length

            match rix' = wix with
            | true -> state <- Writable(items, wix)
            | _ -> state <- ReadWritable(items, wix, rix')

            Some items[rix]
        | _ -> None

    member _.Push(item: 'item) =
        match state with
        | Writable(items, ix) ->
            items[ix] <- item
            let wix = (ix + 1) % items.Length
            state <- ReadWritable(items, wix, ix)
        | ReadWritable(items, wix, rix) ->
            items[wix] <- item
            let wix' = (wix + 1) % items.Length

            match wix' = rix with
            | true -> state <- ReadWritable(items |> doubleSize rix, items.Length, 0)
            | _ -> state <- ReadWritable(items, wix', rix)

module internal Program' =
    module Cmd =
        let internal exec onError (dispatch: Dispatch<'msg>) (cmd: Cmd<'msg>) =
            cmd
            |> List.iter (fun call ->
                try
                    call dispatch
                with ex ->
                    onError ex)

    module Subs = Sub.Internal

    /// Returns a pair: start the program, and stop it from outside the pump for a
    /// caller that has no message to send, such as the reader hitting end of input.
    let runFirstRender (arg: 'arg) (program: Program<'arg, 'model, 'msg, 'view>) =
        // An ugly way to extract properties from the program because they're private
        let init = Program.init program
        let update = Program.update program
        let setState = Program.setState program
        let onError = Program.onError program
        let mutable subscribe = fun _ -> []
        let mutable termination = (fun _ -> false), ignore

        program
        |> Program.mapSubscription (fun f ->
            subscribe <- f
            f)
        |> Program.mapTermination (fun f ->
            termination <- f
            f)
        |> ignore

        let model, cmd = init arg
        let sub = subscribe model
        let toTerminate, terminate = termination
        let rb = RingBuffer 10
        let mutable reentered = false
        let mutable state = model
        let mutable activeSubs = Subs.empty
        let mutable terminated = false
        // Keystrokes, resize notifications and timers each dispatch from their own
        // thread, so the queue and the pump have to be taken one thread at a time.
        let pump = obj ()

        let stop () =
            if not terminated then
                Subs.Fx.stop onError activeSubs
                terminate state
                terminated <- true

        let rec dispatch msg =
            lock pump (fun () ->
                if not terminated then
                    rb.Push msg

                    if not reentered then
                        reentered <- true

                        // An update that throws would otherwise leave the flag set
                        // and every later message would queue behind it for ever.
                        try
                            processMsgs ()
                        finally
                            reentered <- false)

        and processMsgs () =
            let mutable nextMsg = rb.Pop()

            while not terminated && Option.isSome nextMsg do
                let msg = nextMsg.Value

                if toTerminate msg then
                    stop ()
                else
                    let model', cmd' = update msg state
                    let sub' = subscribe model'
                    setState model' dispatch

                    cmd'
                    |> Cmd.exec (fun ex -> onError ($"Error handling the message: %A{msg}", ex)) dispatch

                    state <- model'

                    // Cmd.quit sets its flag while the commands run, which is after
                    // the check above. Without this second look a quit waits for the
                    // next message to arrive before it lands.
                    if toTerminate msg then
                        stop ()
                    else
                        activeSubs <- Subs.diff activeSubs sub' |> Subs.Fx.change onError dispatch
                        nextMsg <- rb.Pop()

        reentered <- true
        setState model dispatch

        let run () =
            lock pump (fun () ->
                try
                    cmd |> Cmd.exec (fun ex -> onError ("Error intitializing:", ex)) dispatch
                    activeSubs <- Subs.diff activeSubs sub |> Subs.Fx.change onError dispatch
                    processMsgs ()
                finally
                    reentered <- false)

        run, (fun () -> lock pump stop)

module internal Sub =
    open System
    open System.Timers

    let timer (intervalMs: float) msg =
        let start dispatch =
            let timer = new Timer(TimeSpan.FromMilliseconds(intervalMs))
            timer.Elapsed.Add(fun _ -> dispatch msg)
            timer.Start()

            { new IDisposable with
                member _.Dispose() =
                    timer.Stop()
                    timer.Dispose() }

        start

/// Carries a program's stop request to Cmd.quit, which the user writes at compile
/// time and so cannot be handed the running program. AsyncLocal keeps it scoped to
/// one program's run and flows into the tasks and threads that run started.
type QuitToken() =
    member val Requested = false with get, set

    static member val private current = new System.Threading.AsyncLocal<QuitToken voption>()

    static member install() =
        let token = QuitToken()
        QuitToken.current.Value <- ValueSome token
        token

    static member clear() = QuitToken.current.Value <- ValueNone

    static member request() =
        match QuitToken.current.Value with
        | ValueSome token -> token.Requested <- true
        | ValueNone -> ()

[<RequireQualifiedAccess>]
module Cmd =
    /// Stop the program after the current message is handled.
    let quit<'msg> : Cmd<'msg> = [ fun _ -> QuitToken.request () ]

    /// Dispatch a message once, after a delay.
    let after (delay: int<ms>) (msg: 'msg) : Cmd<'msg> =
        [ fun dispatch ->
              let timer = new System.Timers.Timer(float (int delay))
              timer.AutoReset <- false

              timer.Elapsed.Add(fun _ ->
                  dispatch msg
                  timer.Dispose())

              timer.Start() ]
