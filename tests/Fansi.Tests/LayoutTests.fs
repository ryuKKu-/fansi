module Fansi.Tests.LayoutTests

open Xunit
open Fansi.Core
open Fansi

[<Fact>]
let ``text measures its own length`` () =
    Assert.Equal((5, 1), Layout.measure (Ui.text "hello") 80 24)

[<Fact>]
let ``text wraps at the available width`` () =
    Assert.Equal((4, 2), Layout.measure (Ui.text "hello") 4 24)

[<Fact>]
let ``an explicit newline starts a new line`` () =
    Assert.Equal((3, 2), Layout.measure (Ui.text "ab\ncde") 80 24)

[<Fact>]
let ``a column is as tall as its children together`` () =
    let node = Ui.col [ Ui.text "a"; Ui.text "b"; Ui.text "c" ]
    Assert.Equal((1, 3), Layout.measure node 80 24)

[<Fact>]
let ``a row is as wide as its children together`` () =
    let node = Ui.row [ Ui.text "ab"; Ui.text "cd" ]
    Assert.Equal((4, 1), Layout.measure node 80 24)

[<Fact>]
let ``a column is as wide as its widest child`` () =
    let node = Ui.col [ Ui.text "a"; Ui.text "wide" ]
    Assert.Equal((4, 2), Layout.measure node 80 24)

[<Fact>]
let ``padding adds to the measured size`` () =
    let node = Ui.col [ Ui.text "ab" ] |> Ui.pad 1
    Assert.Equal((4, 3), Layout.measure node 80 24)

[<Fact>]
let ``a border adds one cell on each side`` () =
    let node = Ui.col [ Ui.text "ab" ] |> Ui.border Single
    Assert.Equal((4, 3), Layout.measure node 80 24)

[<Fact>]
let ``margin adds to the measured size`` () =
    let node = Ui.col [ Ui.text "ab" ] |> Ui.margin 2
    Assert.Equal((6, 5), Layout.measure node 80 24)

[<Fact>]
let ``a fixed child measures at its fixed size, not its content`` () =
    let node = Ui.row [ Ui.text "a very long label" |> Ui.len 4 ]
    Assert.Equal(4, fst (Layout.measure node 80 24))

[<Fact>]
let ``an empty container measures as nothing`` () =
    Assert.Equal((0, 0), Layout.measure Ui.empty 80 24)

[<Fact>]
let ``border thickness is one for every visible style`` () =
    Assert.Equal(0, Layout.Border.thickness NoBorder)
    Assert.Equal(1, Layout.Border.thickness Single)
    Assert.Equal(1, Layout.Border.thickness Rounded)
    Assert.Equal(1, Layout.Border.thickness Ascii)

let private screen w h = { X = 0; Y = 0; Width = w; Height = h }

let private rects (ln: Layout.LayoutNode) =
    ln.Children |> List.map (fun c -> c.Rect)

[<Fact>]
let ``a column stacks its children top to bottom`` () =
    let node = Ui.col [ Ui.text "a"; Ui.text "b" ]
    let laid = Layout.arrange node (screen 10 10)
    Assert.Equal<int list>([ 0; 1 ], rects laid |> List.map (fun r -> r.Y))
    Assert.Equal<int list>([ 0; 0 ], rects laid |> List.map (fun r -> r.X))
    // stretch is the default, so each row spans the full width
    Assert.Equal<int list>([ 10; 10 ], rects laid |> List.map (fun r -> r.Width))

[<Fact>]
let ``a row places its children left to right`` () =
    let node = Ui.row [ Ui.text "ab"; Ui.text "cd" ]
    let laid = Layout.arrange node (screen 10 10)
    Assert.Equal<int list>([ 0; 2 ], rects laid |> List.map (fun r -> r.X))

[<Fact>]
let ``fill children tile the row exactly`` () =
    let node =
        Ui.row [ Ui.text "a" |> Ui.fill 1; Ui.text "b" |> Ui.fill 1; Ui.text "c" |> Ui.fill 1 ]

    let laid = Layout.arrange node (screen 100 10)
    let widths = rects laid |> List.map (fun r -> r.Width)
    Assert.Equal(100, List.sum widths)
    // no gap: each child starts where the previous one ended
    let xs = rects laid |> List.map (fun r -> r.X)
    Assert.Equal<int list>([ 0; 34; 67 ], xs)

[<Fact>]
let ``a fixed sidebar keeps its width and the rest fills`` () =
    let node = Ui.row [ Ui.text "side" |> Ui.len 30; Ui.text "main" |> Ui.fill 1 ]
    let laid = Layout.arrange node (screen 80 10)
    Assert.Equal<int list>([ 30; 50 ], rects laid |> List.map (fun r -> r.Width))

[<Fact>]
let ``a percentage child is sized against the parent's inner width`` () =
    // this is the case the old engine got wrong: it resolved the width, then
    // laid the child out again without it
    let node = Ui.row [ Ui.text "x" |> Ui.pct 25 ]
    let laid = Layout.arrange node (screen 80 10)
    Assert.Equal(20, (List.head (rects laid)).Width)

[<Fact>]
let ``border and padding shrink the area given to children`` () =
    let node = Ui.col [ Ui.text "x" |> Ui.fill 1 ] |> Ui.border Single |> Ui.pad 1
    let laid = Layout.arrange node (screen 10 10)
    let child = List.head (rects laid)
    Assert.Equal(2, child.X)
    Assert.Equal(2, child.Y)
    Assert.Equal(6, child.Height)
    Assert.Equal(6, child.Width)

[<Fact>]
let ``justify center puts the slack on both sides`` () =
    let node = Ui.row [ Ui.text "ab" ] |> Ui.justify Justify.Center
    let laid = Layout.arrange node (screen 10 1)
    Assert.Equal(4, (List.head (rects laid)).X)

[<Fact>]
let ``justify end pushes children to the far edge`` () =
    let node = Ui.row [ Ui.text "ab" ] |> Ui.justify Justify.End
    let laid = Layout.arrange node (screen 10 1)
    Assert.Equal(8, (List.head (rects laid)).X)

[<Fact>]
let ``justify between spreads the slack into the gaps`` () =
    let node = Ui.row [ Ui.text "a"; Ui.text "b" ] |> Ui.justify Justify.Between
    let laid = Layout.arrange node (screen 10 1)
    Assert.Equal<int list>([ 0; 9 ], rects laid |> List.map (fun r -> r.X))

[<Fact>]
let ``justify between leaves no cell over at the far edge`` () =
    let node =
        Ui.row [ Ui.text "a"; Ui.text "b"; Ui.text "c" ] |> Ui.justify Justify.Between

    let laid = Layout.arrange node (screen 20 1)
    let placed = rects laid
    Assert.Equal<int list>([ 0; 10; 19 ], placed |> List.map (fun r -> r.X))
    // the last child must finish flush with the right edge, not one short
    let last = List.last placed
    Assert.Equal(20, last.X + last.Width)

[<Fact>]
let ``align stretch gives children the full cross extent`` () =
    let node = Ui.row [ Ui.text "a" ] |> Ui.align Align.Stretch
    let laid = Layout.arrange node (screen 10 5)
    Assert.Equal(5, (List.head (rects laid)).Height)

[<Fact>]
let ``align start leaves a child at its intrinsic cross size`` () =
    let node = Ui.row [ Ui.text "a" ] |> Ui.align Align.Start
    let laid = Layout.arrange node (screen 10 5)
    Assert.Equal(1, (List.head (rects laid)).Height)

[<Fact>]
let ``an explicit cross constraint beats align stretch`` () =
    let node = Ui.row [ Ui.text "a" |> Ui.cross (Len 2) ] |> Ui.align Align.Stretch
    let laid = Layout.arrange node (screen 10 5)
    Assert.Equal(2, (List.head (rects laid)).Height)

[<Fact>]
let ``align center centres a child across the axis`` () =
    let node = Ui.row [ Ui.text "a" ] |> Ui.align Align.Center
    let laid = Layout.arrange node (screen 10 5)
    Assert.Equal(2, (List.head (rects laid)).Y)

[<Fact>]
let ``a child is clipped to its parent's inner area`` () =
    let node = Ui.col [ Ui.text "x" |> Ui.len 50 ] |> Ui.border Single
    let laid = Layout.arrange node (screen 10 5)
    let child = List.head laid.Children
    Assert.True(child.Clip.Height <= 3)
    Assert.True(child.Clip.Y >= 1)

[<Fact>]
let ``margin offsets a node inside its slot`` () =
    let node = Ui.col [ Ui.text "a" |> Ui.margin 1 ]
    let laid = Layout.arrange node (screen 10 5)
    let child = List.head (rects laid)
    Assert.Equal(1, child.X)
    Assert.Equal(1, child.Y)

[<Fact>]
let ``nested containers keep tiling`` () =
    let node =
        Ui.col
            [ Ui.row [ Ui.text "a" |> Ui.fill 1; Ui.text "b" |> Ui.fill 1 ] |> Ui.fill 1
              Ui.text "status" |> Ui.len 1 ]

    let laid = Layout.arrange node (screen 80 24)
    let top = List.head laid.Children
    Assert.Equal(23, top.Rect.Height)
    Assert.Equal(80, top.Children |> List.sumBy (fun c -> c.Rect.Width))

[<Fact>]
let ``a node with saturated padding measures without wrapping`` () =
    // exact values matter here: a loose ">= 0" check would still pass if outer
    // wrapped negative and max 0 flattened it, which is the bug this pins against
    let maxNode = Ui.col [ Ui.text "x" ] |> Ui.pad System.Int32.MaxValue
    Assert.Equal((System.Int32.MaxValue, System.Int32.MaxValue), Layout.measure maxNode 80 24)

    // Negative padding clamps to zero, so this is the size of the text alone.
    let minNode = Ui.col [ Ui.text "x" ] |> Ui.pad System.Int32.MinValue
    Assert.Equal((1, 1), Layout.measure minNode 80 24)
