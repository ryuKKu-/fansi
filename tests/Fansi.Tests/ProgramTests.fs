module Fansi.Tests.ProgramTests

open System.Threading
open Xunit
open Elmish
open Fansi
open Fansi.Core

[<Fact>]
let ``quit sets the token for the current scope`` () =
    let token = QuitToken.install ()
    Assert.False(token.Requested)

    Cmd.quit |> List.iter (fun call -> call ignore)

    Assert.True(token.Requested)

[<Fact>]
let ``quit without a program in scope does nothing rather than throwing`` () =
    QuitToken.clear ()
    Cmd.quit |> List.iter (fun call -> call ignore)

[<Fact>]
let ``after dispatches its message once the delay passes`` () =
    let got = new ManualResetEventSlim(false)
    let mutable received = 0

    Cmd.after 20<ms> 7
    |> List.iter (fun call ->
        call (fun m ->
            received <- m
            got.Set()))

    Assert.True(got.Wait 2000, "the delayed message never arrived")
    Assert.Equal(7, received)

[<Fact>]
let ``after fires once, not repeatedly`` () =
    let mutable count = 0

    Cmd.after 20<ms> ()
    |> List.iter (fun call -> call (fun () -> Interlocked.Increment(&count) |> ignore))

    Thread.Sleep 200
    Assert.Equal(1, count)

[<Fact>]
let ``the new terminal sequences are what the terminal expects`` () =
    Assert.Equal("\x1b[?2004h", AnsiSequence.enableBracketedPaste)
    Assert.Equal("\x1b[?2004l", AnsiSequence.disableBracketedPaste)
    Assert.Equal("\x1b[?1004h", AnsiSequence.enableFocusReporting)
    Assert.Equal("\x1b[?1004l", AnsiSequence.disableFocusReporting)

/// Drives Program'.runFirstRender directly: messages are strings, the model is a
/// counter, and rendering is stubbed out so only the pump is under test.
let private newPump update onTerminate =
    let token = QuitToken.install ()
    let dispatch = ref (ignore<string>)

    let program =
        Program.mkProgram (fun () -> 0, [ (fun d -> dispatch.Value <- d) ]) update (fun model _ -> model)
        |> Program.withSetState (fun _ _ -> ())
        |> Program.withTermination (fun _ -> token.Requested) onTerminate

    let start, stop = Program'.runFirstRender () program
    start ()
    (fun msg -> dispatch.Value msg), stop

[<Fact>]
let ``a quit from a command lands on the message that asked for it`` () =
    let mutable terminated = false
    let mutable updates = 0

    let update msg model =
        updates <- updates + 1
        model + 1, (if msg = "quit" then Cmd.quit else Cmd.none)

    let dispatch, _ = newPump update (fun _ -> terminated <- true)

    dispatch "quit"

    Assert.True(terminated, "the quit took a second message to land")
    Assert.Equal(1, updates)

[<Fact>]
let ``stopping from outside the pump terminates and drops later messages`` () =
    let mutable terminated = false
    let mutable updates = 0

    let update _ model =
        updates <- updates + 1
        model + 1, Cmd.none

    let dispatch, stop = newPump update (fun _ -> terminated <- true)

    stop ()
    dispatch "too late"

    Assert.True(terminated, "the external stop did not terminate the program")
    Assert.Equal(0, updates)

[<Fact>]
let ``dispatching from several threads loses no messages`` () =
    let mutable handled = 0

    let update _ model =
        // Not interlocked on purpose: the pump has to run one message at a time.
        handled <- handled + 1
        model + 1, Cmd.none

    let dispatch, _ = newPump update ignore

    let threads =
        [ for _ in 1..4 ->
              Thread(
                  ThreadStart(fun () ->
                      for _ in 1..250 do
                          dispatch "tick")
              ) ]

    threads |> List.iter (fun t -> t.Start())
    threads |> List.iter (fun t -> t.Join())

    Assert.Equal(1000, handled)

[<Fact>]
let ``an update that throws does not wedge the pump`` () =
    let mutable handled = 0

    let update msg model =
        handled <- handled + 1

        if msg = "boom" then
            failwith "boom"

        model + 1, Cmd.none

    let dispatch, _ = newPump update ignore

    try
        dispatch "boom"
    with _ ->
        ()

    dispatch "the pump still works"

    Assert.Equal(2, handled)
