module Fansi.Tests.PaintTests

open Xunit
open Fansi.Core
open Fansi.Core.Layout
open Fansi

let private render w h node = Paint.render w h node |> Buffer.toLines

[<Fact>]
let ``text is drawn where it is laid out`` () =
    Assert.Equal<string list>([ "hi  "; "    " ], render 4 2 (Ui.text "hi"))

[<Fact>]
let ``a column draws its children on successive lines`` () =
    Assert.Equal<string list>([ "ab "; "cd " ], render 3 2 (Ui.col [ Ui.text "ab"; Ui.text "cd" ]))

[<Fact>]
let ``a border is drawn around the box`` () =
    let node = Ui.col [ Ui.text "ab" ] |> Ui.border Ascii |> Ui.fill 1
    Assert.Equal<string list>([ "+--+"; "|ab|"; "+--+" ], render 4 3 node)

[<Fact>]
let ``nested borders are both drawn`` () =
    // the old renderer only drew the root box's border
    let inner = Ui.col [ Ui.text "x" ] |> Ui.border Ascii |> Ui.fill 1
    let outer = Ui.col [ inner ] |> Ui.border Ascii |> Ui.fill 1

    Assert.Equal<string list>([ "+---+"; "|+-+|"; "||x||"; "|+-+|"; "+---+" ], render 5 5 outer)

[<Fact>]
let ``a child cannot draw past its parent's border`` () =
    let node = Ui.col [ Ui.text "abcdefghij" ] |> Ui.border Ascii |> Ui.fill 1
    let lines = render 6 3 node
    Assert.Equal("+----+", lines[0])
    Assert.Equal("|abcd|", lines[1])
    Assert.Equal("+----+", lines[2])

[<Fact>]
let ``a child cannot draw past its parent's border horizontally`` () =
    // a long single-line child inside a narrow bordered box: the border's right
    // column must survive, not be overwritten by the child's text
    let node = Ui.row [ Ui.text "abcdefghij" ] |> Ui.border Ascii |> Ui.fill 1
    let lines = render 6 3 node
    Assert.Equal("+----+", lines[0])
    Assert.Equal("|abcd|", lines[1])
    Assert.Equal("+----+", lines[2])

[<Fact>]
let ``padding pushes content inwards`` () =
    let node = Ui.col [ Ui.text "x" ] |> Ui.pad 1 |> Ui.fill 1
    Assert.Equal<string list>([ "   "; " x "; "   " ], render 3 3 node)

[<Fact>]
let ``a later sibling draws over an earlier one`` () =
    // Siblings can't overlap through Ui: the solver tiles exactly and offsets are
    // prefix sums, so two children never share a rect. Drive Paint.node directly
    // against a hand-built layout tree instead, so paint order is actually pinned.
    let rect = { X = 0; Y = 0; Width = 3; Height = 1 }

    let leaf text =
        { Rect = rect
          Clip = rect
          Node = Text(text, Style.Default, Props.Default)
          Children = [] }

    let root =
        { Rect = rect
          Clip = rect
          Node = Container([], Style.Default, Props.Default)
          Children = [ leaf "aaa"; leaf "bbb" ] }

    let buf = Buffer.create 3 1
    Paint.node buf root
    Assert.Equal<string list>([ "bbb" ], Buffer.toLines buf)

[<Fact>]
let ``a background fills the box without erasing text`` () =
    let node = Ui.col [ Ui.text "ab" ] |> Ui.bg Color.Blue |> Ui.fill 1
    let buf = Paint.render 3 1 node
    Assert.Equal(Color.Blue, (Buffer.get buf 0 0).Style.BgColor)
    Assert.Equal(Color.Blue, (Buffer.get buf 2 0).Style.BgColor)
    Assert.Equal("a", (Buffer.get buf 0 0).Symbol)

[<Fact>]
let ``the three panel layout tiles the width`` () =
    let panel label =
        Ui.col [ Ui.text label ] |> Ui.border Ascii |> Ui.fill 1

    let node = Ui.row [ panel "a"; panel "b"; panel "c" ] |> Ui.fill 1
    let lines = render 12 3 node
    Assert.Equal(3, List.length lines)
    Assert.All(lines, fun l -> Assert.Equal(12, l.Length))
    Assert.Equal("+--++--++--+", lines[0])

[<Fact>]
let ``a cross constraint is measured, not only applied at placement`` () =
    // measuring the child against the parent's full width while placing it at
    // Len 3 cut "hello world" down to "hel" and dropped the rest
    let node = Ui.col [ Ui.text "hello world" |> Ui.cross (Len 3) ]
    let lines = render 11 4 node
    Assert.Equal<string list>([ "hel"; "lo "; "wor"; "ld " ], lines |> List.map (fun l -> l.Substring(0, 3)))
    Assert.All(lines, fun l -> Assert.Equal("        ", l.Substring 3))

[<Fact>]
let ``a percentage cross constraint is measured too`` () =
    let node = Ui.col [ Ui.text "hello world" |> Ui.cross (Pct 20) ]
    let lines = render 40 2 node
    Assert.Equal<string list>([ "hello wo"; "rld     " ], lines |> List.map (fun l -> l.Substring(0, 8)))

[<Fact>]
let ``a parent's foreground reaches its children`` () =
    let node = Ui.col [ Ui.text "hi" ] |> Ui.fg Color.Cyan
    let buf = Paint.render 2 1 node
    Assert.Equal(Color.Cyan, (Buffer.get buf 0 0).Style.FgColor)

[<Fact>]
let ``a parent's bold reaches its children`` () =
    let node = Ui.col [ Ui.text "hi" ] |> Ui.bold
    let buf = Paint.render 2 1 node
    Assert.True((Buffer.get buf 0 0).Style.Bold)

[<Fact>]
let ``a child's own foreground beats its parent's`` () =
    let node = Ui.col [ Ui.text "hi" |> Ui.fg Color.Red ] |> Ui.fg Color.Cyan
    let buf = Paint.render 2 1 node
    Assert.Equal(Color.Red, (Buffer.get buf 0 0).Style.FgColor)

[<Fact>]
let ``a double border draws its own glyphs`` () =
    let node = Ui.col [ Ui.text "x" ] |> Ui.border Double |> Ui.fill 1

    Assert.Equal<string list>([ "╔═╗"; "║x║"; "╚═╝" ], render 3 3 node)

[<Fact>]
let ``a heavy border draws its own glyphs`` () =
    let node = Ui.col [ Ui.text "x" ] |> Ui.border Heavy |> Ui.fill 1

    Assert.Equal<string list>([ "┏━┓"; "┃x┃"; "┗━┛" ], render 3 3 node)

[<Fact>]
let ``a single border draws its own glyphs`` () =
    let node = Ui.col [ Ui.text "x" ] |> Ui.border Single |> Ui.fill 1

    Assert.Equal<string list>([ "┌─┐"; "│x│"; "└─┘" ], render 3 3 node)

[<Fact>]
let ``a rounded border draws its own glyphs`` () =
    let node = Ui.col [ Ui.text "x" ] |> Ui.border Rounded |> Ui.fill 1

    Assert.Equal<string list>([ "╭─╮"; "│x│"; "╰─╯" ], render 3 3 node)

[<Fact>]
let ``negative padding does not eat the border`` () =
    let node = Ui.col [ Ui.text "x" ] |> Ui.border Single |> Ui.pad (-1)

    Assert.Equal<string list>([ "┌─┐"; "│x│"; "└─┘" ], render 3 3 node)

[<Fact>]
let ``negative margin does not paint outside the node's space`` () =
    let node =
        Ui.row
            [ Ui.col [ Ui.text "a" ] |> Ui.border Single |> Ui.margin (-1) |> Ui.len 3
              Ui.text "b" ]

    Assert.Equal<string list>([ "┌─┐b"; "│a│ "; "└─┘ " ], render 4 3 node)

[<Fact>]
let ``wide text wraps by cells inside a border`` () =
    // Two chars fit three columns, but two wide characters need four cells.
    let node = Ui.col [ Ui.text "字字" ] |> Ui.border Ascii |> Ui.fill 1
    Assert.Equal<string list>([ "+---+"; "|字 |"; "|字 |"; "+---+" ], render 5 4 node)

[<Fact>]
let ``an escape sequence in text is drawn, not obeyed`` () =
    Assert.Equal<string list>([ "a[2Jb " ], render 6 1 (Ui.text "a\x1b[2Jb"))

[<Fact>]
let ``a line keeps each run's own style`` () =
    let buf = Paint.render 4 1 (Ui.line [ Ui.text "ab"; Ui.text "cd" |> Ui.bold ])
    Assert.Equal<string list>([ "abcd" ], Buffer.toLines buf)
    Assert.False((Buffer.get buf 1 0).Style.Bold)
    Assert.True((Buffer.get buf 2 0).Style.Bold)

[<Fact>]
let ``a container inside a line gives only its text and style`` () =
    let inner =
        Ui.row [ Ui.text "x" |> Ui.fg Color.Red; Ui.text "y" ]
        |> Ui.border Single
        |> Ui.pad 3

    let buf = Paint.render 6 1 (Ui.line [ Ui.text "> "; inner ])
    Assert.Equal<string list>([ "> xy  " ], Buffer.toLines buf)
    Assert.Equal(Color.Red, (Buffer.get buf 2 0).Style.FgColor)

[<Fact>]
let ``a line wraps across its runs`` () =
    let buf = Paint.render 4 2 (Ui.line [ Ui.text "abc"; Ui.text "def" |> Ui.bold ])
    Assert.Equal<string list>([ "abcd"; "ef  " ], Buffer.toLines buf)
    Assert.True((Buffer.get buf 3 0).Style.Bold)
    Assert.True((Buffer.get buf 0 1).Style.Bold)

[<Fact>]
let ``a line's own style reaches its runs`` () =
    let buf = Paint.render 2 1 (Ui.line [ Ui.text "a" ] |> Ui.fg Color.Red)
    Assert.Equal(Color.Red, (Buffer.get buf 0 0).Style.FgColor)

[<Fact>]
let ``a background inside a line carries to its text`` () =
    let buf = Paint.render 2 1 (Ui.line [ Ui.row [ Ui.text "x" ] |> Ui.bg Color.Blue ])
    Assert.Equal(Color.Blue, (Buffer.get buf 0 0).Style.BgColor)

let private titled width title =
    Ui.col [ Ui.text "x" ]
    |> Ui.border Single
    |> Ui.title title
    |> Ui.fill 1
    |> render width 3
    |> List.head

[<Fact>]
let ``a title sits in the top border`` () =
    Assert.Equal("┌ Tasks ───┐", titled 12 "Tasks")

[<Fact>]
let ``a title is cut to fit`` () =
    Assert.Equal("┌ Task ┐", titled 8 "Tasks")

[<Fact>]
let ``a box five wide still shows one character`` () = Assert.Equal("┌ T ┐", titled 5 "Tasks")

[<Fact>]
let ``a box too narrow for a title keeps a plain border`` () = Assert.Equal("┌──┐", titled 4 "Tasks")

[<Fact>]
let ``a wide character at the cut is dropped`` () = Assert.Equal("┌ ab ─┐", titled 7 "ab字")

[<Fact>]
let ``no border means no title`` () =
    Assert.Equal<string list>([ "x   " ], render 4 1 (Ui.text "x" |> Ui.title "T"))

[<Fact>]
let ``a rich title keeps its style and takes the node's colour`` () =
    let node =
        Ui.col [ Ui.text "x" ]
        |> Ui.border Single
        |> Ui.titleWith (Ui.line [ Ui.text "T" |> Ui.bold ])
        |> Ui.fg Color.Cyan
        |> Ui.fill 1

    let buf = Paint.render 8 3 node
    let cell = Buffer.get buf 2 0
    Assert.Equal("T", cell.Symbol)
    Assert.True(cell.Style.Bold)
    Assert.Equal(Color.Cyan, cell.Style.FgColor)
