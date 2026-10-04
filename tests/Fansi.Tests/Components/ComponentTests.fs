module Fansi.Tests.ComponentTests

open System
open Xunit
open Fansi
open Fansi.Core

let private render w h node = Paint.render w h node |> Buffer.toLines

[<Fact>]
let ``the timer shows the remaining time`` () =
    let model, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    Assert.Equal("00:00:05.000", (render 20 1 (TimerComponent.view model)).Head.TrimEnd())

[<Fact>]
let ``the button shows its label inside a border`` () =
    let model, _ = ButtonComponent.init "OK"
    let lines = render 10 3 (ButtonComponent.view false model)
    Assert.Contains("OK", lines[1])
    Assert.StartsWith("\u250c", lines[0])

[<Fact>]
let ``a focused button has a rounded border`` () =
    let model, _ = ButtonComponent.init "OK"
    Assert.StartsWith("\u256d", (render 10 3 (ButtonComponent.view true model))[0])

[<Fact>]
let ``the list marks the focused item`` () =
    let model, _ = ListComponent.init [ "one"; "two" ] id 2
    Assert.Equal<string list>([ "\u25b8 one"; "  two" ], render 5 2 (ListComponent.view model))

[<Fact>]
let ``a wide focus marker keeps the items lined up`` () =
    let model, _ = ListComponent.init [ "a"; "b" ] id 2
    let model = { model with FocusedIndicator = "✅" }
    let buf = Paint.render 4 2 (ListComponent.view model)
    Assert.Equal("a", (Buffer.get buf 2 0).Symbol)
    Assert.Equal("b", (Buffer.get buf 2 1).Symbol)

[<Fact>]
let ``the list moves and selects on real keys`` () =
    let model, _ = ListComponent.init [ "one"; "two"; "three" ] id 2

    let press key m =
        fst (ListComponent.update (ListComponent.KeyInput(KeyEvent.plain key)) m)

    let moved = model |> press Key.Down |> press Key.Down
    Assert.Equal(2, moved.FocusItemIndex)
    Assert.Equal(1, moved.ViewportOffset)
    Assert.Equal(Some "three", ListComponent.selectedItem (press Key.Enter moved))
    Assert.Equal(1, (press Key.Up moved).FocusItemIndex)

[<Fact>]
let ``the checkbox glyph follows its state`` () =
    let model, _ = CheckboxComponent.init "done"
    Assert.Equal("\u2610 done", (render 10 1 (CheckboxComponent.view model)).Head.TrimEnd())

    let ticked, _ = CheckboxComponent.update CheckboxComponent.Toggle model
    Assert.Equal("\u2611 done", (render 10 1 (CheckboxComponent.view ticked)).Head.TrimEnd())

[<Fact>]
let ``the progress bar fills in proportion`` () =
    let model = ProgressBarComponent.init 10 |> ProgressBarComponent.setProgress 0.5
    let line = (render 20 1 (ProgressBarComponent.view model)).Head
    Assert.Equal(String.replicate 5 "\u2588" + String.replicate 5 "\u2591" + " 50%", line.TrimEnd())

[<Fact>]
let ``the spinner shows its current frame and label`` () =
    let model, _ = SpinnerComponent.init SpinnerComponent.Line 100<ms> "loading"
    Assert.Equal("| loading", (render 20 1 (SpinnerComponent.view model)).Head.TrimEnd())

    let next, _ = SpinnerComponent.update (SpinnerComponent.Tick model.Id) model
    Assert.Equal("/ loading", (render 20 1 (SpinnerComponent.view next)).Head.TrimEnd())

[<Fact>]
let ``two timers get their own id and subscription`` () =
    let a, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    let b, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    Assert.NotEqual(a.Id, b.Id)
    Assert.Equal<string list list>([ [ "fansi"; "timer"; string a.Id ] ], TimerComponent.subscribe a |> List.map fst)

    Assert.NotEqual<string list list>(
        TimerComponent.subscribe a |> List.map fst,
        TimerComponent.subscribe b |> List.map fst
    )

[<Fact>]
let ``a timer ignores another timer's tick`` () =
    let a, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    let b, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    let ticked, _ = TimerComponent.update (TimerComponent.TickMsg b.Id) a
    Assert.Equal(a.Timeout, ticked.Timeout)
    let own, _ = TimerComponent.update (TimerComponent.TickMsg a.Id) a
    Assert.Equal(TimeSpan.FromSeconds 4.0, own.Timeout)

[<Fact>]
let ``a stopped timer does not subscribe`` () =
    let a, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    Assert.Empty(TimerComponent.subscribe { a with Running = false })

[<Fact>]
let ``a paused timer ignores its own tick`` () =
    let a, _ = TimerComponent.init 1000<ms> (TimeSpan.FromSeconds 5.0)
    let paused = { a with Running = false }
    let ticked, cmd = TimerComponent.update (TimerComponent.TickMsg paused.Id) paused
    Assert.Equal(paused.Timeout, ticked.Timeout)
    Assert.Empty(cmd)

[<Fact>]
let ``two spinners get their own subscription and ignore each other's tick`` () =
    let a, _ = SpinnerComponent.init SpinnerComponent.Line 100<ms> ""
    let b, _ = SpinnerComponent.init SpinnerComponent.Line 100<ms> ""

    Assert.Equal<string list list>(
        [ [ "fansi"; "spinner"; string a.Id ] ],
        SpinnerComponent.subscribe a |> List.map fst
    )

    Assert.NotEqual<string list list>(
        SpinnerComponent.subscribe a |> List.map fst,
        SpinnerComponent.subscribe b |> List.map fst
    )

    let ticked, _ = SpinnerComponent.update (SpinnerComponent.Tick b.Id) a
    Assert.Equal(a.Frame, ticked.Frame)

[<Fact>]
let ``a wide value stays inside its text input's box`` () =
    let input, _ = TextInputComponent.init ()

    let input, _ =
        TextInputComponent.update (TextInputComponent.SetValue "日本語😀テキスト") { input with Width = 8 }

    let node =
        Ui.row
            [ TextInputComponent.view true input |> Ui.border Single |> Ui.len 12
              Ui.text "R" |> Ui.fill 1 ]

    let buf = Paint.render 20 3 node
    Assert.Equal("│", (Buffer.get buf 11 1).Symbol)
    Assert.Equal("R", (Buffer.get buf 12 0).Symbol)

    // The cursor cell is drawn inside the box, not lost under the border.
    Assert.Contains(Color.Cyan, [ for x in 1..10 -> (Buffer.get buf x 1).Style.BgColor ])
