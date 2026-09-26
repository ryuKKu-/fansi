module Fansi.Tests.GeometryTests

open Xunit
open FsCheck.Xunit
open Fansi.Core

let rect x y w h = { X = x; Y = y; Width = w; Height = h }

[<Fact>]
let ``intersect of overlapping rects is the overlap`` () =
    let a = rect 0 0 10 10
    let b = rect 5 5 10 10
    Assert.Equal(rect 5 5 5 5, Rect.intersect a b)

[<Fact>]
let ``intersect of disjoint rects is empty`` () =
    let a = rect 0 0 5 5
    let b = rect 10 10 5 5
    Assert.True(Rect.isEmpty (Rect.intersect a b))

[<Property>]
let ``intersect never grows either rect`` (x: int) (y: int) (w: int) (h: int) =
    let a = rect 0 0 20 20
    let b = rect x y w h
    let r = Rect.intersect a b
    r.Width <= max 0 a.Width && r.Height <= max 0 a.Height

[<Fact>]
let ``deflate removes padding from all four sides`` () =
    let r = rect 0 0 10 10

    let e =
        { Top = 1
          Right = 2
          Bottom = 3
          Left = 4 }

    Assert.Equal(rect 4 1 4 6, Rect.deflate e r)

[<Fact>]
let ``deflate never produces a negative size`` () =
    let r = rect 0 0 2 2
    let deflated = Rect.deflate (Edges.All 5) r
    Assert.Equal(0, deflated.Width)
    Assert.Equal(0, deflated.Height)

[<Fact>]
let ``contains is inclusive of the origin and exclusive of the far edge`` () =
    let r = rect 2 3 4 5
    Assert.True(Rect.contains 2 3 r)
    Assert.True(Rect.contains 5 7 r)
    Assert.False(Rect.contains 6 3 r)
    Assert.False(Rect.contains 2 8 r)

[<Fact>]
let ``edges helpers set the expected sides`` () =
    Assert.Equal(
        { Top = 0
          Right = 2
          Bottom = 0
          Left = 2 },
        Edges.X 2
    )

    Assert.Equal(
        { Top = 3
          Right = 0
          Bottom = 3
          Left = 0 },
        Edges.Y 3
    )

    Assert.Equal(4, (Edges.All 2).Horizontal)
    Assert.Equal(4, (Edges.All 2).Vertical)

[<Fact>]
let ``edge sums saturate instead of wrapping negative`` () =
    let e =
        { Top = System.Int32.MaxValue
          Right = System.Int32.MaxValue
          Bottom = System.Int32.MaxValue
          Left = System.Int32.MaxValue }

    Assert.Equal(System.Int32.MaxValue, e.Horizontal)
    Assert.Equal(System.Int32.MaxValue, e.Vertical)

[<Fact>]
let ``deflating by a saturated edge empties the rect rather than growing it`` () =
    let r =
        { X = 0
          Y = 0
          Width = 10
          Height = 10 }

    let deflated = Rect.deflate (Edges.All System.Int32.MaxValue) r
    Assert.Equal(0, deflated.Width)
    Assert.Equal(0, deflated.Height)

[<Fact>]
let ``deflateBy shrinks every side by the same amount`` () =
    let r = { X = 2; Y = 3; Width = 10; Height = 8 }
    Assert.Equal({ X = 3; Y = 4; Width = 8; Height = 6 }, Rect.deflateBy 1 r)

[<Fact>]
let ``saturating sums clamp at both ends of the int range`` () =
    Assert.Equal(System.Int32.MaxValue, Saturating.add System.Int32.MaxValue System.Int32.MaxValue)
    Assert.Equal(System.Int32.MinValue, Saturating.add System.Int32.MinValue System.Int32.MinValue)
    Assert.Equal(System.Int32.MaxValue, Saturating.add3 System.Int32.MaxValue 1 1)
    Assert.Equal(System.Int32.MinValue, Saturating.add3 System.Int32.MinValue -1 -1)
    Assert.Equal(7, Saturating.add 3 4)
    Assert.Equal(9, Saturating.add3 2 3 4)

[<Fact>]
let ``add3 clamps on the whole total rather than pairwise`` () =
    // pins the example in add3's doc comment, which was wrong once already
    Assert.Equal(-1, Saturating.add (Saturating.add System.Int32.MaxValue System.Int32.MaxValue) System.Int32.MinValue)

    Assert.Equal(
        System.Int32.MaxValue - 1,
        Saturating.add3 System.Int32.MaxValue System.Int32.MaxValue System.Int32.MinValue
    )

[<Fact>]
let ``saturating differences clamp at both ends of the int range`` () =
    Assert.Equal(System.Int32.MaxValue, Saturating.sub System.Int32.MaxValue System.Int32.MinValue)
    Assert.Equal(System.Int32.MinValue, Saturating.sub System.Int32.MinValue System.Int32.MaxValue)
    Assert.Equal(-1, Saturating.sub 3 4)

[<Fact>]
let ``edge sums do not wrap when the total goes below the int range`` () =
    let e =
        { Top = System.Int32.MinValue
          Right = -1
          Bottom = System.Int32.MinValue
          Left = System.Int32.MinValue }

    Assert.Equal(System.Int32.MinValue, e.Horizontal)
    Assert.Equal(System.Int32.MinValue, e.Vertical)
