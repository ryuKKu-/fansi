namespace Fansi.Core

type Edges =
    { Top: int
      Right: int
      Bottom: int
      Left: int }

    member this.Horizontal = this.Left + this.Right
    member this.Vertical = this.Top + this.Bottom

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

    member this.Right = this.X + this.Width
    member this.Bottom = this.Y + this.Height

    static member Empty = { X = 0; Y = 0; Width = 0; Height = 0 }

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
          Width = max 0 (r.Width - e.Horizontal)
          Height = max 0 (r.Height - e.Vertical) }

    let deflateBy n (r: Rect) = deflate (Edges.All n) r
