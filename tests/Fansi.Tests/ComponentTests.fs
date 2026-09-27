module Fansi.Tests.ComponentTests

open System
open Xunit
open Fansi
open Fansi.Core

let private render w h node = Paint.render w h node |> Buffer.toLines

[<Fact>]
let ``two text inputs get their own cursor id`` () =
    let a, _ = TextInputComponent.init ()
    let b, _ = TextInputComponent.init ()
    Assert.NotEqual(a.Cursor.Id, b.Cursor.Id)

[<Fact>]
let ``the timer shows the remaining time`` () =
    let model, _ = TimerComponent.init 1 1000.0 (TimeSpan.FromSeconds 5.0)
    Assert.Equal("\u23f1 00:00:05.000", (render 20 1 (TimerComponent.view model)).Head.TrimEnd())

[<Fact>]
let ``the text input shows its prompt and its value`` () =
    let model, _ = TextInputComponent.init ()
    let model = TextInputComponent.insertSpan ("hi".AsSpan()) model
    Assert.StartsWith("> hi", (render 10 1 (TextInputComponent.view model)).Head)

[<Fact>]
let ``a plain character is inserted into a focused text input`` () =
    let model, _ = TextInputComponent.init ()
    let model = { model with Focused = true }

    let model, _ =
        TextInputComponent.update (TextInputComponent.KeyInput(KeyEvent.plain (Key.Char 'a'))) model

    Assert.Equal("a", model.Value.ToString())

[<Fact>]
let ``ctrl+letter does not insert into a focused text input`` () =
    let model, _ = TextInputComponent.init ()
    let model = { model with Focused = true }

    let model, _ =
        TextInputComponent.update
            (TextInputComponent.KeyInput
                { Key = Key.Char 'a'
                  Ctrl = true
                  Alt = false
                  Shift = false })
            model

    Assert.Equal("", model.Value.ToString())

[<Fact>]
let ``the button shows its label inside a border`` () =
    let model, _ = ButtonComponent.init "OK"
    let lines = render 10 3 (ButtonComponent.view model)
    Assert.Contains("OK", lines[1])
    Assert.StartsWith("\u250c", lines[0])

[<Fact>]
let ``the list marks the focused item`` () =
    let model, _ = ListComponent.init [ "one"; "two" ] id 2
    Assert.Equal<string list>([ "\u25b8 one"; "  two" ], render 5 2 (ListComponent.view model))

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
    let model, _ = SpinnerComponent.init SpinnerComponent.Line 100.0 "loading"
    Assert.Equal("| loading", (render 20 1 (SpinnerComponent.view model)).Head.TrimEnd())

    let next, _ = SpinnerComponent.update SpinnerComponent.Tick model
    Assert.Equal("/ loading", (render 20 1 (SpinnerComponent.view next)).Head.TrimEnd())

[<Fact>]
let ``insertSpan fits a span that is within the limit`` () =
    let model, _ = TextInputComponent.init ()
    let model = { model with CharLimit = 10 }
    let model = TextInputComponent.insertSpan ("abcdefgh".AsSpan()) model
    Assert.Equal("abcdefgh", model.Value.ToString())

[<Fact>]
let ``insertSpan truncates to the room that is left`` () =
    let model, _ = TextInputComponent.init ()
    let model = { model with CharLimit = 10 }
    let model = TextInputComponent.insertSpan ("abcde".AsSpan()) model
    let model = TextInputComponent.insertSpan ("fghijklmn".AsSpan()) model
    Assert.Equal("abcdefghij", model.Value.ToString())

[<Fact>]
let ``insertSpan inserts nothing when the limit is already reached`` () =
    let model, _ = TextInputComponent.init ()
    let model = { model with CharLimit = 3 }
    let model = TextInputComponent.insertSpan ("abc".AsSpan()) model
    let model = TextInputComponent.insertSpan ("de".AsSpan()) model
    Assert.Equal("abc", model.Value.ToString())

[<Fact>]
let ``insertSpan ignores the limit when it is zero`` () =
    let model, _ = TextInputComponent.init ()

    let model =
        TextInputComponent.insertSpan ("a longer string than any limit".AsSpan()) model

    Assert.Equal("a longer string than any limit", model.Value.ToString())
