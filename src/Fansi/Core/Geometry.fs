namespace Fansi.Core

/// Sums that cannot wrap. Margins, padding and sizes come from user code as
/// unconstrained ints, so a total can leave the int range. F# truncates an
/// out-of-range int64 to its low 32 bits, and a total that wraps negative makes
/// Rect.deflate grow a rect instead of shrinking it, so these clamp instead.
module Saturating =

    let private clamp (total: int64) =
        total
        |> min (int64 System.Int32.MaxValue)
        |> max (int64 System.Int32.MinValue)
        |> int

    let add a b = clamp (int64 a + int64 b)

    /// Clamps once on the combined total, not on each pairwise step, so it is not
    /// the same as `add (add a b) c`: chaining `add` can clamp partway through and
    /// lose the rest of the range, e.g. `add (add MaxValue MaxValue) MinValue` is
    /// `-1`, while `add3 MaxValue MaxValue MinValue` is `MaxValue - 1`.
    let add3 a b c = clamp (int64 a + int64 b + int64 c)

    let sub a b = clamp (int64 a - int64 b)

type Edges =
    { Top: int
      Right: int
      Bottom: int
      Left: int }

    /// Left plus right. The individual edges come from user code and are unconstrained
    /// ints; a wrapped total would make Rect.deflate grow a rect instead of shrinking it.
    member this.Horizontal = Saturating.add this.Left this.Right

    /// Top plus bottom. Saturated for the same reason as Horizontal.
    member this.Vertical = Saturating.add this.Top this.Bottom

    static member Zero =
        { Top = 0
          Right = 0
          Bottom = 0
          Left = 0 }

    static member All n =
        { Top = n
          Right = n
          Bottom = n
          Left = n }

    static member X n =
        { Top = 0
          Right = n
          Bottom = 0
          Left = n }

    static member Y n =
        { Top = n
          Right = 0
          Bottom = n
          Left = 0 }

type Rect =
    { X: int
      Y: int
      Width: int
      Height: int }

    member this.Right = Saturating.add this.X this.Width
    member this.Bottom = Saturating.add this.Y this.Height

module Rect =
    let isEmpty (r: Rect) = r.Width <= 0 || r.Height <= 0

    let contains x y (r: Rect) =
        x >= r.X && x < r.Right && y >= r.Y && y < r.Bottom

    let intersect (a: Rect) (b: Rect) =
        let x = max a.X b.X
        let y = max a.Y b.Y
        let right = min a.Right b.Right
        let bottom = min a.Bottom b.Bottom

        { X = x
          Y = y
          Width = max 0 (right - x)
          Height = max 0 (bottom - y) }

    let deflate (e: Edges) (r: Rect) =
        { X = r.X + e.Left
          Y = r.Y + e.Top
          Width = max 0 (Saturating.sub r.Width e.Horizontal)
          Height = max 0 (Saturating.sub r.Height e.Vertical) }

    let deflateBy n (r: Rect) = deflate (Edges.All n) r
