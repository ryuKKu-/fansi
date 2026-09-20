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
    Assert.Equal('a', (Buffer.get buf 0 0).Char)

[<Fact>]
let ``the three panel layout tiles the width`` () =
    let panel label =
        Ui.col [ Ui.text label ] |> Ui.border Ascii |> Ui.fill 1

    let node = Ui.row [ panel "a"; panel "b"; panel "c" ] |> Ui.fill 1
    let lines = render 12 3 node
    Assert.Equal(3, List.length lines)
    Assert.All(lines, fun l -> Assert.Equal(12, l.Length))
    Assert.Equal("+--++--++--+", lines[0])
