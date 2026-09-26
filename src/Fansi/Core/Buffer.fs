namespace Fansi.Core

type Cell =
    { Char: char
      Style: Style }

    static member Empty = { Char = ' '; Style = Style.Default }

type Buffer =
    { Width: int
      Height: int
      Cells: Cell array }

module Buffer =
    let create width height =
        let w = max 0 width
        let h = max 0 height

        { Width = w
          Height = h
          Cells = Array.create (w * h) Cell.Empty }

    let private index (b: Buffer) x y = y * b.Width + x

    let private inside (b: Buffer) x y =
        x >= 0 && x < b.Width && y >= 0 && y < b.Height

    let get (b: Buffer) x y =
        if inside b x y then b.Cells[index b x y] else Cell.Empty

    /// Write one cell. Anything outside the clip rect or off the buffer is dropped,
    /// which is what keeps a child from drawing over its parent.
    let set (b: Buffer) (clip: Rect) x y cell =
        if inside b x y && Rect.contains x y clip then
            b.Cells[index b x y] <- cell

    /// Write text. A style whose background is Default keeps whatever background is
    /// already in the cell, so a container's background shows through its children.
    let writeText (b: Buffer) (clip: Rect) x y (style: Style) (text: string) =
        text
        |> Seq.iteri (fun i ch ->
            let px = x + i

            let merged =
                if style.BgColor = Color.Default then
                    { style with
                        BgColor = (get b px y).Style.BgColor }
                else
                    style

            set b clip px y { Char = ch; Style = merged })

    let fillRect (b: Buffer) (clip: Rect) (area: Rect) style =
        for y in area.Y .. area.Bottom - 1 do
            for x in area.X .. area.Right - 1 do
                let existing = get b x y
                set b clip x y { existing with Style = style }

    let toLines (b: Buffer) =
        [ for y in 0 .. b.Height - 1 -> System.String(Array.init b.Width (fun x -> b.Cells[index b x y].Char)) ]

module Paint =
    open Fansi.Core.Layout

    let private drawBorder (buf: Buffer) (clip: Rect) (rect: Rect) (bc: Border.BorderChars) style =
        if rect.Width > 0 && rect.Height > 0 then
            let middle = String.replicate (max 0 (rect.Width - 2)) (string bc.Horizontal)
            Buffer.writeText buf clip rect.X rect.Y style (string bc.TopLeft + middle + string bc.TopRight)

            for y in rect.Y + 1 .. rect.Bottom - 2 do
                Buffer.set buf clip rect.X y { Char = bc.Vertical; Style = style }
                Buffer.set buf clip (rect.Right - 1) y { Char = bc.Vertical; Style = style }

            if rect.Height > 1 then
                Buffer.writeText
                    buf
                    clip
                    rect.X
                    (rect.Bottom - 1)
                    style
                    (string bc.BottomLeft + middle + string bc.BottomRight)

    /// Combine a node's own style with the one it inherits from its parent.
    /// A foreground only carries down when the child leaves it Default. The four
    /// attributes are OR-ed, so a child cannot switch off bold inside a bold parent;
    /// that is a limitation we accept, since Style has no "off" value to say it with.
    /// Background is absent on purpose: Buffer.writeText already keeps a parent's
    /// background visible under its children.
    let private cascade (parent: Style) (own: Style) =
        { own with
            FgColor =
                if own.FgColor = Color.Default then
                    parent.FgColor
                else
                    own.FgColor
            Bold = own.Bold || parent.Bold
            Italic = own.Italic || parent.Italic
            Underline = own.Underline || parent.Underline
            Strikethrough = own.Strikethrough || parent.Strikethrough }

    let rec private paint (buf: Buffer) (parent: Style) (ln: LayoutNode) =
        let p = Node.props ln.Node
        let s = cascade parent (Node.style ln.Node)

        if s.BgColor <> Color.Default then
            Buffer.fillRect buf ln.Clip ln.Rect s

        match Border.chars p.Border with
        | Some bc -> drawBorder buf ln.Clip ln.Rect bc s
        | None -> ()

        match ln.Node with
        | Text(t, _, _) ->
            let area = contentRect ln

            wrapText t area.Width
            |> List.truncate (max 0 area.Height)
            |> List.iteri (fun i line -> Buffer.writeText buf ln.Clip area.X (area.Y + i) s line)
        | Container _ -> ln.Children |> List.iter (paint buf s)

    /// Draw a laid-out tree. Each node paints its own background and border, then
    /// its children, so later siblings end up on top.
    let node (buf: Buffer) (ln: LayoutNode) = paint buf Style.Default ln

    /// Lay out and draw a tree into a fresh buffer of the given size.
    let render width height (tree: Node) =
        let buf = Buffer.create width height

        let area =
            { X = 0
              Y = 0
              Width = max 0 width
              Height = max 0 height }

        node buf (arrange tree area)
        buf
