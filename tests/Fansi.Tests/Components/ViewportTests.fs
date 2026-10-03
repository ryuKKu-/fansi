module Fansi.Tests.ViewportTests

open Xunit
open FsCheck.Xunit
open Fansi
open Fansi.Core

module V = ViewportComponent

let private render w h node =
    Paint.render w h node |> Buffer.toLines |> List.map (fun l -> l.TrimEnd())

let private numbered n =
    [ for i in 1..n -> Ui.text $"line {i}" ]

let private viewport w h content = V.init w h content |> fst

let private send msg m = V.update msg m |> fst

let private key k m = send (V.KeyInput(KeyEvent.plain k)) m

let private ctrl c m =
    send (V.KeyInput(KeyEvent.ctrl (Key.Char c))) m

let private mouse button m =
    send
        (V.MouseInput
            { Button = button
              Action = MouseAction.Press
              X = 0
              Y = 0
              Ctrl = false
              Alt = false
              Shift = false })
        m

let private at offset (m: V.Model) = { m with YOffset = offset }

[<Fact>]
let ``a viewport starts at the top`` () =
    let m = viewport 10 3 (numbered 10)
    Assert.Equal(0, m.YOffset)
    Assert.Equal(3, m.WheelStep)
    Assert.Equal<string list>([ "line 1"; "line 2"; "line 3" ], render 10 3 (V.view m))

[<Fact>]
let ``each message moves by its own amount`` () =
    let m = viewport 10 4 (numbered 20)
    Assert.Equal(1, (send V.LineDown m).YOffset)
    Assert.Equal(4, (send V.PageDown m).YOffset)
    Assert.Equal(2, (send V.HalfPageDown m).YOffset)
    Assert.Equal(7, (send V.LineUp (at 8 m)).YOffset)
    Assert.Equal(4, (send V.PageUp (at 8 m)).YOffset)
    Assert.Equal(6, (send V.HalfPageUp (at 8 m)).YOffset)

[<Fact>]
let ``scrolling stops at both ends`` () =
    let m = viewport 10 4 (numbered 20)
    Assert.Equal(0, (send V.LineUp m).YOffset)
    Assert.Equal(0, (send V.PageUp (at 2 m)).YOffset)
    let bottom = send V.Bottom m
    Assert.Equal(16, bottom.YOffset)
    Assert.Equal(16, (send V.LineDown bottom).YOffset)
    Assert.Equal(16, (send V.PageDown (at 15 m)).YOffset)
    Assert.Equal(0, (send V.Top bottom).YOffset)

[<Fact>]
let ``keys scroll the viewport`` () =
    let m = viewport 10 4 (numbered 20)
    Assert.Equal(1, (key Key.Down m).YOffset)
    Assert.Equal(4, (key Key.PageDown m).YOffset)
    Assert.Equal(4, (key (Key.Char ' ') m).YOffset)
    Assert.Equal(2, (ctrl 'd' m).YOffset)
    let bottom = key Key.End m
    Assert.Equal(16, bottom.YOffset)
    Assert.Equal(15, (key Key.Up bottom).YOffset)
    Assert.Equal(12, (key Key.PageUp bottom).YOffset)
    Assert.Equal(14, (ctrl 'u' bottom).YOffset)
    Assert.Equal(0, (key Key.Home bottom).YOffset)

[<Fact>]
let ``other keys change nothing`` () =
    let m = viewport 10 4 (numbered 20) |> at 3
    Assert.Equal(m, key (Key.Char 'x') m)
    Assert.Equal(m, key Key.Left m)
    Assert.Equal(m, key Key.Enter m)

[<Fact>]
let ``the wheel scrolls by its step`` () =
    let m = viewport 10 4 (numbered 20)
    Assert.Equal(3, (mouse MouseButton.ScrollDown m).YOffset)
    Assert.Equal(5, (mouse MouseButton.ScrollDown { m with WheelStep = 5 }).YOffset)
    Assert.Equal(2, (mouse MouseButton.ScrollUp (at 5 m)).YOffset)
    Assert.Equal(at 5 m, mouse MouseButton.Left (at 5 m))

[<Fact>]
let ``a hand-set offset is clamped before use`` () =
    let m = viewport 10 3 (numbered 20)
    Assert.Equal<string list>([ "line 18"; "line 19"; "line 20" ], render 10 3 (V.view (at 99 m)))
    Assert.True(V.atBottom (at 99 m))
    Assert.Equal(16, (send V.LineUp (at 99 m)).YOffset)
    Assert.Equal<string list>([ "line 1"; "line 2"; "line 3" ], render 10 3 (V.view (at -5 m)))
    Assert.True(V.atTop (at -5 m))

[<Fact>]
let ``content shorter than the height never scrolls`` () =
    let m = viewport 10 5 (numbered 2)
    Assert.Equal(0, (send V.PageDown m).YOffset)
    Assert.Equal(0, (send V.Bottom m).YOffset)
    Assert.Equal<string list>([ "line 1"; "line 2"; ""; ""; "" ], render 10 5 (V.view m))

[<Fact>]
let ``a viewport with no height shows nothing and never scrolls`` () =
    for height in [ 0; -2 ] do
        let m = viewport 10 height (numbered 5)
        Assert.Equal(0, (send V.LineDown m).YOffset)
        Assert.Equal(0, (send V.Bottom m).YOffset)
        Assert.Empty(Node.children (V.view m))
        Assert.True(V.atTop m)
        Assert.True(V.atBottom m)
        Assert.Equal(100, V.scrollPercent m)

[<Fact>]
let ``a width of zero does not wrap`` () =
    let m = viewport 0 2 [ Ui.text (String.replicate 50 "a") ]
    Assert.Equal<string list>([ String.replicate 50 "a"; "" ], render 60 2 (V.view m))

[<Fact>]
let ``empty content is at the top and the bottom`` () =
    let m = viewport 10 3 []
    Assert.True(V.atTop m)
    Assert.True(V.atBottom m)
    Assert.Equal(100, V.scrollPercent m)
    Assert.Equal<string list>([ ""; ""; "" ], render 10 3 (V.view m))

[<Fact>]
let ``the percentage follows the offset`` () =
    let m = viewport 10 4 (numbered 20)
    Assert.Equal(0, V.scrollPercent m)
    Assert.True(V.atTop m)
    Assert.False(V.atBottom m)
    Assert.Equal(50, V.scrollPercent (at 8 m))
    Assert.False(V.atTop (at 8 m))
    Assert.False(V.atBottom (at 8 m))
    Assert.Equal(100, V.scrollPercent (at 16 m))
    Assert.True(V.atBottom (at 16 m))

[<Fact>]
let ``long lines wrap to the width and scroll by rows`` () =
    let m = viewport 4 2 [ Ui.text "abcdefgh"; Ui.text "ij" ]
    Assert.Equal<string list>([ "abcd"; "efgh" ], render 4 2 (V.view m))
    Assert.Equal<string list>([ "efgh"; "ij" ], render 4 2 (V.view (send V.LineDown m)))

[<Fact>]
let ``a styled run keeps its style`` () =
    let m =
        viewport 10 1 [ Ui.line [ Ui.text "ok "; Ui.text "no" |> Ui.bold |> Ui.fg Color.Red ] ]

    let buffer = Paint.render 10 1 (V.view m)
    let cell = Buffer.get buffer 3 0
    Assert.Equal("n", cell.Symbol)
    Assert.True(cell.Style.Bold)
    Assert.Equal(Color.Red, cell.Style.FgColor)
    Assert.False((Buffer.get buffer 0 0).Style.Bold)

[<Fact>]
let ``a wide character that does not fit moves whole to the next row`` () =
    let m = viewport 3 2 [ Ui.text "ab字" ]
    Assert.Equal<string list>([ "ab"; "字" ], render 3 2 (V.view m))

[<Fact>]
let ``an empty paragraph stays a blank row`` () =
    let m = viewport 10 3 [ Ui.text "a"; Ui.text ""; Ui.text "b" ]
    Assert.Equal<string list>([ "a"; ""; "b" ], render 10 3 (V.view m))

[<Fact>]
let ``a newline inside a paragraph starts a row`` () =
    let m = viewport 10 3 [ Ui.text "a\nb"; Ui.text "c" ]
    Assert.Equal<string list>([ "a"; "b"; "c" ], render 10 3 (V.view m))

[<Fact>]
let ``tabs are dropped like other control characters`` () =
    let m = viewport 10 1 [ Ui.text "a\tb" ]
    Assert.Equal<string list>([ "ab" ], render 10 1 (V.view m))

let private paragraphsOf (texts: string list) = texts |> List.map Ui.text

[<Fact>]
let ``set text makes one paragraph per line`` () =
    let m = viewport 10 3 [] |> V.setText "a\nb\nc"
    Assert.Equal(3, m.Content.Length)
    Assert.Equal<string list>([ "a"; "b"; "c" ], render 10 3 (V.view m))

[<Fact>]
let ``new content at the bottom keeps the view at the bottom`` () =
    let m = viewport 10 3 (numbered 3) |> V.setContent (numbered 5)
    Assert.Equal(2, m.YOffset)
    let fromEmpty = viewport 10 3 [] |> V.setContent (numbered 10)
    Assert.Equal(7, fromEmpty.YOffset)

[<Fact>]
let ``new content after scrolling up keeps the offset`` () =
    let m = viewport 10 3 (numbered 10) |> send V.LineDown |> V.setContent (numbered 20)
    Assert.Equal(1, m.YOffset)

[<Fact>]
let ``shorter content clamps the offset`` () =
    let m = viewport 10 3 (numbered 10) |> at 5 |> V.setContent (numbered 4)
    Assert.Equal(1, m.YOffset)

[<Fact>]
let ``a narrower width keeps the top paragraph at the top`` () =
    let content = paragraphsOf [ "aaaaaaaa"; "bbbbbbbb"; "cccccccc"; "dddddddd" ]
    let m = viewport 8 2 content |> send V.LineDown |> V.setSize 4 2
    Assert.Equal(2, m.YOffset)
    Assert.Equal<string list>([ "bbbb"; "bbbb" ], render 4 2 (V.view m))

[<Fact>]
let ``a resize at the bottom stays at the bottom`` () =
    let content = paragraphsOf [ "aaaaaaaa"; "bbbbbbbb"; "cccccccc"; "dddddddd" ]
    let m = viewport 8 2 content |> send V.Bottom |> V.setSize 4 2
    Assert.Equal(6, m.YOffset)
    Assert.Equal<string list>([ "dddd"; "dddd" ], render 4 2 (V.view m))

[<Fact>]
let ``a taller view clamps the offset`` () =
    let m = viewport 10 3 (numbered 10) |> send V.LineDown |> V.setSize 10 20
    Assert.Equal(0, m.YOffset)
    Assert.Equal(20, m.Height)

[<Fact>]
let ``a long log follows its end`` () =
    let m = viewport 20 3 [] |> V.setContent (numbered 10000)
    Assert.Equal(9997, m.YOffset)
    Assert.Equal<string list>([ "line 9998"; "line 9999"; "line 10000" ], render 20 3 (V.view m))

let private messages =
    [| V.LineUp
       V.LineDown
       V.PageUp
       V.PageDown
       V.HalfPageUp
       V.HalfPageDown
       V.Top
       V.Bottom |]

[<Property>]
let ``the offset stays in range and the view keeps its size``
    (texts: string list)
    (width: int)
    (height: int)
    (steps: int list)
    =
    let content = texts |> List.map (fun t -> Ui.text (if isNull t then "" else t))

    let step m (k: int) =
        match abs (k % 10) with
        | 8 -> V.setContent (List.truncate (abs (k % 5)) content) m
        | 9 -> V.setSize (abs (k % 7) + 1) (abs (k % 5)) m
        | i -> send messages[i] m

    let m =
        steps
        |> List.fold step (viewport (abs (width % 12) + 1) (abs (height % 6)) content)

    let rows =
        m.Content
        |> List.sumBy (fun node -> Runs.wrap (Runs.ofNode Style.Default node) m.Width |> List.length)

    let last = if m.Height <= 0 then 0 else max 0 (rows - m.Height)
    let shown = Node.children (V.view m)

    m.YOffset >= 0
    && m.YOffset <= last
    && shown.Length = max 0 m.Height
    && shown
       |> List.forall (fun row -> Runs.width (Runs.ofNode Style.Default row) <= m.Width)

let private tricky =
    [| "a"; "b"; "\n"; "日"; "👋"; "́"; "\t"; "\u0007"; " "; "xyz"; "é" |]

[<Property>]
let ``the row counter agrees with the wrapper`` (picks: int list) (width: int) =
    let text =
        picks |> List.map (fun p -> tricky[abs (p % tricky.Length)]) |> String.concat ""

    let width = abs (width % 20) + 1
    let node = Ui.text text
    V.rowsIn width node = List.length (Runs.wrap (Runs.ofNode Style.Default node) width)

[<Fact>]
let ``a large log scrolls and follows without redoing the whole wrap`` () =
    let m = viewport 40 10 (numbered 20000)
    let scrolled = [ 1..200 ] |> List.fold (fun m _ -> send V.LineDown m) m
    Assert.Equal(200, scrolled.YOffset)
    Assert.Equal(1, V.scrollPercent scrolled)
    Assert.Equal<string list>([ "line 201"; "line 202" ], render 40 10 (V.view scrolled) |> List.take 2)

    let grown =
        [ 1..500 ]
        |> List.fold
            (fun (m: V.Model) i -> V.setContent (m.Content @ [ Ui.text $"line {20000 + i}" ]) m)
            (send V.Bottom m)

    Assert.Equal(20490, grown.YOffset)
    Assert.True(V.atBottom grown)
    Assert.Equal("line 20500", (render 40 10 (V.view grown) |> List.last))

[<Fact>]
let ``set text drops one trailing newline`` () =
    let two = viewport 10 3 [] |> V.setText "a\nb\n"
    Assert.Equal(2, two.Content.Length)
    let blank = viewport 10 3 [] |> V.setText "a\n\n"
    Assert.Equal(2, blank.Content.Length)
    Assert.Equal<string list>([ "a"; ""; "" ], render 10 3 (V.view blank))
    Assert.Equal(1, (viewport 10 3 [] |> V.setText "a").Content.Length)

[<Fact>]
let ``a resize keeps the paragraph when the top row is inside it`` () =
    let content = paragraphsOf [ "aaaaaaaa"; "bbbbbbbb"; "cccccccc"; "dddddddd" ]
    let m = viewport 4 2 content |> at 3 |> V.setSize 8 2
    Assert.Equal(1, m.YOffset)
    Assert.Equal<string list>([ "bbbbbbbb"; "cccccccc" ], render 8 2 (V.view m))

[<Fact>]
let ``a wider view has fewer rows and clamps the offset`` () =
    let content = paragraphsOf [ "aaaaaaaa"; "bbbbbbbb"; "cccccccc" ]
    let m = viewport 4 2 content |> at 4 |> V.setSize 8 2
    Assert.Equal(1, m.YOffset)
    Assert.True(V.atBottom m)
