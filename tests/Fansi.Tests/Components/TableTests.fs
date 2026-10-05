module Fansi.Tests.TableTests

open Xunit
open FsCheck.Xunit
open Fansi
open Fansi.Core

module T = TableComponent

type private Proc = { Name: string; Cpu: int }

let private render w h node =
    Paint.render w h node |> Buffer.toLines |> List.map (fun l -> l.TrimEnd())

let private column title width : T.Column =
    { T.Column.Title = title
      T.Column.Width = width }

let private procs n =
    [ for i in 1..n -> { Name = $"p{i}"; Cpu = i } ]

let private procColumns = [ column "Name" (Len 6); column "CPU" (Fill 1) ]

let private table w h rows =
    let m, _ =
        T.init procColumns (fun (p: Proc) -> [ Ui.text p.Name; Ui.text (string p.Cpu) ]) w h rows

    { m with Border = NoBorder }

let private strings w h columns (rows: string list list) =
    let m, _ = T.init columns (List.map Ui.text) w h rows
    { m with Border = NoBorder }

let private send msg m = T.update msg m |> fst

let private key k m = send (T.KeyInput(KeyEvent.plain k)) m

let private mouse button m =
    send
        (T.MouseInput
            { Button = button
              Action = MouseAction.Press
              X = 0
              Y = 0
              Ctrl = false
              Alt = false
              Shift = false })
        m

let private presses keys m =
    keys |> List.fold (fun m k -> key k m) m

[<Fact>]
let ``columns share the width and the header shows the titles`` () =
    // 12 cells less one gap: Name takes 6, CPU fills the other 5.
    Assert.Equal<string list>([ "Name   CPU"; "p1     1"; "p2     2"; "" ], render 12 4 (T.view (table 12 3 (procs 2))))

[<Fact>]
let ``a percentage column takes its share of the room after the gaps`` () =
    let m =
        strings 21 1 [ column "A" (Len 4); column "B" (Pct 50); column "C" (Fill 1) ] []

    // 21 cells less 2 gaps leaves 19: A takes 4, B 50% of 19 = 9, C the other 6.
    let header = (render 21 2 (T.view m))[0]
    Assert.Equal('A', header[0])
    Assert.Equal('B', header[5])
    Assert.Equal('C', header[15])

[<Fact>]
let ``the header is bold`` () =
    let buffer = Paint.render 12 2 (T.view (table 12 1 (procs 1)))
    Assert.True((Buffer.get buffer 0 0).Style.Bold)

[<Fact>]
let ``a title longer than its column is cut`` () =
    let m = strings 0 0 [ column "Status" (Len 3) ] []
    Assert.Equal<string list>([ "Sta" ], render 10 1 (T.view m))

[<Fact>]
let ``a wide character that would cross a column edge is left out`` () =
    let m = strings 0 1 [ column "A" (Len 3); column "B" (Len 2) ] [ [ "ab字"; "x" ] ]
    let buffer = Paint.render 10 2 (T.view m)
    Assert.Equal("x", (Buffer.get buffer 4 1).Symbol)
    Assert.Equal<string list>([ "A   B"; "ab  x" ], render 10 2 (T.view m))

[<Fact>]
let ``missing cells are blank and extra cells are ignored`` () =
    let m =
        strings 0 2 [ column "One" (Len 4); column "T" (Len 1) ] [ [ "only" ]; [ "a"; "b"; "c" ] ]

    Assert.Equal<string list>([ "One  T"; "only"; "a    b" ], render 10 3 (T.view m))

[<Fact>]
let ``keys move the cursor and stop at both ends`` () =
    let m = table 10 3 (procs 10)
    Assert.Equal(1, (key Key.Down m).Cursor)
    Assert.Equal(3, (key Key.PageDown m).Cursor)
    let last = key Key.End m
    Assert.Equal(9, last.Cursor)
    Assert.Equal(8, (key Key.Up last).Cursor)
    Assert.Equal(6, (key Key.PageUp last).Cursor)
    Assert.Equal(9, (key Key.Down last).Cursor)
    Assert.Equal(0, (key Key.Home last).Cursor)
    Assert.Equal(0, (key Key.Up m).Cursor)

[<Fact>]
let ``the wheel moves one row and other events change nothing`` () =
    let m = table 10 3 (procs 10)
    Assert.Equal(1, (mouse MouseButton.ScrollDown m).Cursor)
    Assert.Equal(0, (mouse MouseButton.ScrollUp (mouse MouseButton.ScrollDown m)).Cursor)
    let clicked = mouse MouseButton.Left (key Key.Down m)
    Assert.Equal(1, clicked.Cursor)
    let typed = key (Key.Char 'x') (key Key.Down m)
    Assert.Equal(1, typed.Cursor)
    Assert.Equal(0, typed.Offset)

[<Fact>]
let ``scrolling moves the offset only as far as the cursor needs`` () =
    let m = table 10 3 (procs 10)
    let down = presses [ Key.Down; Key.Down; Key.Down ] m
    Assert.Equal(3, down.Cursor)
    Assert.Equal(1, down.Offset)
    let back = key Key.Up down
    Assert.Equal(2, back.Cursor)
    Assert.Equal(1, back.Offset)
    let top = presses [ Key.Up; Key.Up ] back
    Assert.Equal(0, top.Offset)
    let last = key Key.End m
    Assert.Equal(7, last.Offset)
    Assert.Equal<string list>([ "Name   CPU"; "p8     8"; "p9     9"; "p10    10" ], render 12 4 (T.view last))

[<Fact>]
let ``fewer rows clamp the cursor`` () =
    let m = table 10 3 (procs 10) |> key Key.End |> T.setRows (procs 4)
    Assert.Equal(3, m.Cursor)
    Assert.Equal(1, m.Offset)
    Assert.Equal(Some "p4", T.selectedRow m |> Option.map (fun p -> p.Name))

[<Fact>]
let ``a shorter table scrolls to keep the cursor`` () =
    let m =
        table 10 3 (procs 10)
        |> presses [ Key.Down; Key.Down; Key.Down; Key.Down; Key.Down ]

    Assert.Equal(3, m.Offset)
    let shorter = T.setSize 10 2 m
    Assert.Equal(4, shorter.Offset)
    Assert.Equal(2, shorter.Height)

[<Fact>]
let ``a table with no rows has no selection`` () =
    let m = table 10 3 []
    Assert.Equal(None, T.selectedRow m)
    Assert.Equal(0, (key Key.Down m).Cursor)
    Assert.Equal<string list>([ "Name   CPU"; ""; ""; "" ], render 12 4 (T.view m))

[<Fact>]
let ``a table with no columns draws nothing`` () =
    let m = strings 10 3 [] [ [ "a" ] ]
    Assert.Empty(Node.children (T.view m))

[<Fact>]
let ``a table with no height shows only the header`` () =
    let m = table 10 0 (procs 5)
    Assert.Equal(1, (Node.children (T.view m)).Length)
    Assert.Equal(0, (key Key.Down m).Offset)

[<Fact>]
let ``a width of zero gives each column what it asks for`` () =
    Assert.Equal<string list>([ "Name   CPU"; "p1     1" ], render 20 2 (T.view (table 0 1 (procs 1))))

[<Fact>]
let ``a hand-set cursor out of range still draws`` () =
    let m =
        { table 10 3 (procs 10) with
            Cursor = 99
            Offset = -4 }

    Assert.Equal<string list>([ "Name   CPU"; "p8     8"; "p9     9"; "p10    10" ], render 12 4 (T.view m))
    Assert.Equal(Some "p10", T.selectedRow m |> Option.map (fun p -> p.Name))

[<Fact>]
let ``the cursor row is highlighted across the whole width`` () =
    let buffer = Paint.render 12 4 (T.view (table 12 3 (procs 3)))

    for x in 0..11 do
        Assert.Equal(Color.Cyan, (Buffer.get buffer x 1).Style.BgColor)

    Assert.Equal(Color.Default, (Buffer.get buffer 0 2).Style.BgColor)

[<Fact>]
let ``the selected row follows the cursor`` () =
    let m = table 10 3 (procs 3)
    Assert.Equal(Some "p1", T.selectedRow m |> Option.map (fun p -> p.Name))
    Assert.Equal(Some "p2", T.selectedRow (key Key.Down m) |> Option.map (fun p -> p.Name))

[<Fact>]
let ``no row is wider than a very small width`` () =
    let m =
        strings 1 1 [ column "A" (Fill 1); column "B" (Fill 1); column "C" (Fill 1) ] [ [ "a"; "b"; "c" ] ]

    for row in Node.children (T.view m) do
        Assert.True(Runs.width (Runs.ofNode Style.Default row) <= 1)

let private styles = [| NoBorder; Single; Double; Rounded; Heavy; Ascii |]

let private moves =
    [| Key.Up; Key.Down; Key.PageUp; Key.PageDown; Key.Home; Key.End |]

[<Property>]
let ``the cursor stays in range and on screen``
    (names: string list)
    (width: int)
    (height: int)
    (border: int)
    (steps: int list)
    =
    let rows = names |> List.map (fun n -> [ (if isNull n then "" else n); "x" ])

    let columns = [ column "A" (Len 3); column "B" (Fill 1); column "C" (Pct 30) ]

    let style = styles[abs (border % styles.Length)]

    let start =
        { strings (abs (width % 30)) (abs (height % 6)) columns rows with
            Border = style }

    let m = steps |> List.fold (fun m k -> key moves[abs (k % moves.Length)] m) start

    let shown = Node.children (T.view m)
    let count = rows.Length

    let inRange =
        if count = 0 then
            m.Cursor = 0
        else
            m.Cursor >= 0 && m.Cursor < count

    let onScreen =
        m.Height <= 0
        || count = 0
        || (m.Offset <= m.Cursor && m.Cursor < m.Offset + m.Height)

    let fits =
        m.Width <= 0
        || shown
           |> List.forall (fun row -> Runs.width (Runs.ofNode Style.Default row) <= m.Width)

    inRange
    && onScreen
    && shown.Length = (if style = NoBorder then 1 else 4) + max 0 m.Height
    && fits

let private bordered w h columns (rows: string list list) =
    let m, _ = T.init columns (List.map Ui.text) w h rows
    m

let private ab = [ column "A" (Len 3); column "B" (Len 2) ]

[<Fact>]
let ``a new table draws a single outer box`` () =
    let m = bordered 8 2 ab [ [ "x"; "y" ] ]

    Assert.Equal<string list>(
        [ "┌──────┐"; "│A   B │"; "├──────┤"; "│x   y │"; "│      │"; "└──────┘" ],
        render 10 6 (T.view m)
    )

[<Theory>]
[<InlineData("Double", "╔══════╗", "╠══════╣", "╚══════╝", "║")>]
[<InlineData("Rounded", "╭──────╮", "├──────┤", "╰──────╯", "│")>]
[<InlineData("Heavy", "┏━━━━━━┓", "┣━━━━━━┫", "┗━━━━━━┛", "┃")>]
[<InlineData("Ascii", "+------+", "+------+", "+------+", "|")>]
let ``each style draws its own corners and tees``
    (name: string, top: string, sep: string, bottom: string, bar: string)
    =
    let style =
        match name with
        | "Double" -> Double
        | "Rounded" -> Rounded
        | "Heavy" -> Heavy
        | _ -> Ascii

    let m =
        { bordered 8 1 ab [] with
            Border = style }

    let lines = render 8 5 (T.view m)
    Assert.Equal<string list>([ top; $"{bar}A   B {bar}"; sep; $"{bar}      {bar}"; bottom ], lines)

[<Fact>]
let ``columns share the width less the borders`` () =
    let m = bordered 12 0 [ column "A" (Len 3); column "B" (Fill 1) ] []
    // 12 less 2 borders and 1 gap leaves 9: A takes 3, B the other 6.
    Assert.Equal<string list>(
        [ "┌──────────┐"; "│A   B     │"; "├──────────┤"; "└──────────┘" ],
        render 12 4 (T.view m)
    )

[<Fact>]
let ``no line is wider than a tiny width`` () =
    let m =
        bordered 3 2 [ column "A" (Fill 1); column "B" (Fill 1); column "C" (Fill 1) ] [ [ "a"; "b"; "c" ] ]

    let rows = Node.children (T.view m)
    Assert.Equal(6, rows.Length)

    for row in rows do
        Assert.True(Runs.width (Runs.ofNode Style.Default row) <= 3)

[<Fact>]
let ``missing rows are blank inside the box`` () =
    let m = bordered 8 2 ab [ [ "x"; "y" ] ]
    let lines = render 8 6 (T.view m)
    Assert.Equal("│x   y │", lines[3])
    Assert.Equal("│      │", lines[4])

[<Fact>]
let ``a width of zero wraps the borders round the asked widths`` () =
    let m = bordered 0 1 [ column "Name" (Fill 1); column "C" (Len 3) ] [ [ "x"; "y" ] ]

    Assert.Equal<string list>(
        [ "┌────────┐"; "│Name C  │"; "├────────┤"; "│x    y  │"; "└────────┘" ],
        render 20 5 (T.view m)
    )

[<Fact>]
let ``the border colour applies to border cells only`` () =
    let m =
        { bordered 8 1 ab [ [ "x"; "y" ] ] with
            BorderColor = Color.BrightBlack }

    let coloured = Paint.render 8 5 (T.view m)
    Assert.Equal(Color.BrightBlack, (Buffer.get coloured 0 0).Style.FgColor)
    Assert.Equal(Color.BrightBlack, (Buffer.get coloured 7 1).Style.FgColor)
    Assert.Equal(Color.BrightBlack, (Buffer.get coloured 7 4).Style.FgColor)
    Assert.Equal(Color.Default, (Buffer.get coloured 1 1).Style.FgColor)

    let plain = Paint.render 8 5 (T.view (bordered 8 1 ab []))
    Assert.Equal(Color.Default, (Buffer.get plain 0 0).Style.FgColor)

[<Fact>]
let ``a cell keeps its own colour inside a coloured border`` () =
    let m, _ =
        T.init ab (fun (_: string) -> [ Ui.text "x" |> Ui.fg Color.Green; Ui.text "y" ]) 8 1 [ "r" ]

    let buffer = Paint.render 8 5 (T.view { m with BorderColor = Color.Red })
    Assert.Equal(Color.Green, (Buffer.get buffer 1 3).Style.FgColor)
    Assert.Equal(Color.Red, (Buffer.get buffer 0 3).Style.FgColor)

[<Fact>]
let ``the cursor row background covers the inner width but not the bars`` () =
    let m = bordered 8 2 ab [ [ "x"; "y" ]; [ "z"; "w" ] ]
    let buffer = Paint.render 8 6 (T.view m)

    for x in 1..6 do
        Assert.Equal(Color.Cyan, (Buffer.get buffer x 3).Style.BgColor)

    Assert.Equal(Color.Default, (Buffer.get buffer 0 3).Style.BgColor)
    Assert.Equal(Color.Default, (Buffer.get buffer 7 3).Style.BgColor)

    Assert.Equal(Color.Default, (Buffer.get buffer 0 4).Style.BgColor)
    Assert.Equal(Color.Default, (Buffer.get buffer 0 2).Style.BgColor)
