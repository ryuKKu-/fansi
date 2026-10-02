module Fansi.Tests.SampleTests

open System
open Xunit
open Fansi
open Fansi.Core

module LayoutSample = Fansi.Samples.Layout

let render w h node = Paint.render w h node |> Buffer.toLines

let press key model update =
    update (KeyPress(KeyEvent.plain key)) model |> fst

let sizes = [ 80, 24; 40, 12; 20, 6; 5, 3; 1, 1 ]

let private pageNamed title =
    { LayoutSample.Page = LayoutSample.pages |> List.findIndex (fun p -> p.Title = title) }

let private assertFills w h (lines: string list) =
    Assert.Equal(h, lines.Length)
    Assert.All(lines, fun line -> Assert.Equal(w, Width.ofString line))

[<Fact>]
let ``there are layout pages to show`` () =
    Assert.True(LayoutSample.pages.Length >= 10)

[<Fact>]
let ``every layout page renders at any size`` () =
    for i in 0 .. LayoutSample.pages.Length - 1 do
        for w, h in sizes do
            render w h (LayoutSample.view { LayoutSample.Page = i }) |> assertFills w h

[<Fact>]
let ``right moves to the next page`` () =
    let model = press Key.Right { LayoutSample.Page = 0 } LayoutSample.update
    Assert.Equal(1, model.Page)

[<Fact>]
let ``left on the first page wraps to the last`` () =
    let model = press Key.Left { LayoutSample.Page = 0 } LayoutSample.update
    Assert.Equal(LayoutSample.pages.Length - 1, model.Page)

[<Fact>]
let ``right on the last page wraps to the first`` () =
    let last = { LayoutSample.Page = LayoutSample.pages.Length - 1 }
    Assert.Equal(0, (press Key.Right last LayoutSample.update).Page)

[<Fact>]
let ``home and end jump to the first and last page`` () =
    let middle = { LayoutSample.Page = 3 }
    Assert.Equal(0, (press Key.Home middle LayoutSample.update).Page)
    Assert.Equal(LayoutSample.pages.Length - 1, (press Key.End middle LayoutSample.update).Page)

// The header takes row 0 and the note rows 1 and 2, so the demo starts on row 3.

[<Fact>]
let ``the len and fill page draws a twelve column sidebar`` () =
    let row = (render 40 12 (LayoutSample.view (pageNamed "Len and Fill")))[3]
    Assert.Equal('┌', row[0])
    Assert.Equal('┐', row[11])
    Assert.Equal('┌', row[12])
    Assert.Equal('┐', row[39])

[<Fact>]
let ``the pct and ratio page draws a quarter and a third`` () =
    // pct 25 of 40 is 10, ratio 1 3 of 40 is 13, and the fill gets the other 17.
    let row = (render 40 12 (LayoutSample.view (pageNamed "Pct and Ratio")))[3]
    Assert.Equal('┐', row[9])
    Assert.Equal('┌', row[10])
    Assert.Equal('┐', row[22])
    Assert.Equal('┌', row[23])

[<Fact>]
let ``the shrinking page takes the missing space from the last box`` () =
    // Three len 30 boxes in 70 columns: the last one gives up 20.
    let row = (render 70 12 (LayoutSample.view (pageNamed "Shrinking")))[3]
    Assert.Equal('┐', row[29])
    Assert.Equal('┐', row[59])
    Assert.Equal('┌', row[60])
    Assert.Equal('┐', row[69])

module Dashboard = Fansi.Samples.Dashboard

let private dashboard () = Dashboard.init () |> fst

let private typeText (text: string) model =
    text |> Seq.fold (fun m c -> press (Key.Char c) m Dashboard.update) model

let private focusInput model = press Key.Tab model Dashboard.update

[<Fact>]
let ``the dashboard renders at any size`` () =
    for w, h in sizes do
        render w h (Dashboard.view (dashboard ())) |> assertFills w h

[<Fact>]
let ``the three panels share the width by their constraints`` () =
    let lines = render 80 24 (Dashboard.view (dashboard ()))
    Assert.Equal('┏', lines[0][0])
    Assert.Equal('┓', lines[0][79])
    // The clock starts focused, so its border is the rounded one.
    Assert.Equal('╭', lines[1][1])
    Assert.Equal('╮', lines[1][19])
    Assert.Equal('┌', lines[1][20])
    Assert.Equal('┐', lines[1][52])
    Assert.Equal('┌', lines[1][53])
    Assert.Equal('┐', lines[1][78])
    Assert.Equal("Clock", lines[1].Substring(3, 5))

[<Fact>]
let ``tab moves the rounded border to the next panel`` () =
    let lines = render 80 24 (Dashboard.view (focusInput (dashboard ())))
    Assert.Equal('┌', lines[1][1])
    Assert.Equal('╭', lines[1][20])

[<Fact>]
let ``the focus ring wraps both ways`` () =
    let start = dashboard ()
    let thrice = start |> focusInput |> focusInput |> focusInput
    Assert.Equal(Dashboard.Clock, Focus.current thrice.Focus)

    let back =
        Dashboard.update
            (KeyPress
                { KeyEvent.plain Key.Tab with
                    Shift = true })
            start
        |> fst

    Assert.Equal(Dashboard.Tasks, Focus.current back.Focus)

[<Fact>]
let ``enter adds the typed task and clears the input`` () =
    let model = dashboard () |> focusInput |> typeText "buy milk"
    let model = press Key.Enter model Dashboard.update
    Assert.Equal("buy milk", List.last model.Tasks.Items)
    Assert.Equal("", model.Input.Value)

[<Fact>]
let ``enter on an empty input adds nothing`` () =
    let before = dashboard () |> focusInput
    let after = press Key.Enter (typeText "   " before) Dashboard.update
    Assert.Equal<string list>(before.Tasks.Items, after.Tasks.Items)

[<Fact>]
let ``typing still works after the input is cleared`` () =
    let model = dashboard () |> focusInput |> typeText "first"
    let model = press Key.Enter model Dashboard.update |> typeText "next"
    Assert.Equal("next", model.Input.Value)

[<Fact>]
let ``the checkbox keeps the text after adding`` () =
    let start = dashboard ()
    let onTasks = start |> focusInput |> focusInput
    let ticked = press (Key.Char ' ') onTasks Dashboard.update
    Assert.True(ticked.KeepText.Checked)

    let model = ticked |> focusInput |> focusInput |> typeText "keep me"
    let model = press Key.Enter model Dashboard.update
    Assert.Equal("keep me", model.Input.Value)
    Assert.Equal("keep me", List.last model.Tasks.Items)

[<Fact>]
let ``space in the input types a space and nothing else`` () =
    let model = dashboard () |> focusInput |> typeText "a b"
    Assert.Equal("a b", model.Input.Value)
    Assert.False(model.KeepText.Checked)
    Assert.True(model.Timer.Running)

[<Fact>]
let ``enter in the task list selects the focused task`` () =
    let onTasks = dashboard () |> focusInput |> focusInput
    let model = press Key.Down onTasks Dashboard.update
    let model = press Key.Enter model Dashboard.update
    Assert.Equal(Some(List.item 1 model.Tasks.Items), ListComponent.selectedItem model.Tasks)

[<Fact>]
let ``a paste goes into the input only when it has focus`` () =
    let onClock = Dashboard.update (Paste "pasted") (dashboard ()) |> fst
    Assert.Equal("", onClock.Input.Value)

    let onInput = Dashboard.update (Paste "pasted") (focusInput (dashboard ())) |> fst
    Assert.Equal("pasted", onInput.Input.Value)

[<Fact>]
let ``a task added past the visible rows scrolls into view`` () =
    let add task model =
        press Key.Enter (typeText task model) Dashboard.update

    let model = dashboard () |> focusInput |> add "a" |> add "b" |> add "c"
    let lines = render 80 24 (Dashboard.view model)
    Assert.Contains(lines, fun line -> line.Contains "▸ c")
    Assert.Equal(model.Tasks.Items.Length - 1, model.Tasks.FocusItemIndex)

[<Fact>]
let ``the seeded tasks fit on one row each at 80 columns`` () =
    let lines = render 80 24 (Dashboard.view (dashboard ())) |> List.toArray
    let first = lines |> Array.findIndex (fun line -> line.Contains "▸ layout sample")
    Assert.Contains("dashboard", lines[first + 1])
    Assert.Contains("dead helpers", lines[first + 2])

[<Fact>]
let ``space on a finished clock starts it again`` () =
    let m = dashboard ()

    let finished =
        { m with
            Timer =
                { m.Timer with
                    Timeout = TimeSpan.Zero
                    Running = false }
            Progress = ProgressBarComponent.setProgress 1.0 m.Progress }

    let model = press (Key.Char ' ') finished Dashboard.update
    Assert.True(model.Timer.Running)
    Assert.Equal(TimeSpan.FromMinutes 1.0, model.Timer.Timeout)
    Assert.Equal(0.0, model.Progress.Progress)

[<Fact>]
let ``the text page lines up wide characters and keeps each run's style`` () =
    let buf = Paint.render 40 12 (LayoutSample.view (pageNamed "Text"))
    let lines = Buffer.toLines buf
    Assert.StartsWith("Status: ok - 3 warnings", lines[3])
    Assert.True((Buffer.get buf 8 3).Style.Bold)
    Assert.Equal(Color.Green, (Buffer.get buf 8 3).Style.FgColor)
    Assert.Equal("日", (Buffer.get buf 0 4).Symbol)
    Assert.True((Buffer.get buf 1 4).Continuation)
    Assert.Equal("a", (Buffer.get buf 17 4).Symbol)
    Assert.StartsWith("╭ Title ", lines[5])
