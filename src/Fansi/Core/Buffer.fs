namespace Fansi.Core

type Cell =
    {
        Symbol: string
        Style: Style
        /// The right half of a wide character. The cell before it holds the character.
        Continuation: bool
    }

    static member Empty =
        { Symbol = " "
          Style = Style.Default
          Continuation = false }

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
            let i = index b x y

            // Writing over either half of a wide character blanks the other half, so
            // the terminal is never left with half a glyph.
            if b.Cells[i].Continuation && x > 0 then
                b.Cells[i - 1] <- { b.Cells[i - 1] with Symbol = " " }

            if not b.Cells[i].Continuation && x + 1 < b.Width && b.Cells[i + 1].Continuation then
                b.Cells[i + 1] <-
                    { b.Cells[i + 1] with
                        Symbol = " "
                        Continuation = false }

            b.Cells[i] <- cell

    /// Write text. A style whose background is Default keeps whatever background is
    /// already in the cell, so a container's background shows through its children.
    let writeText (b: Buffer) (clip: Rect) x y (style: Style) (text: string) =
        let fits px =
            inside b px y && Rect.contains px y clip

        let styled px =
            if style.BgColor = Color.Default then
                { style with
                    BgColor = (get b px y).Style.BgColor }
            else
                style

        let mutable px = x

        for g in Width.glyphs text do
            if g.Cells = 2 && not (fits px && fits (px + 1)) then
                // Only one half would show, and a terminal cannot draw half a glyph.
                set
                    b
                    clip
                    px
                    y
                    { Symbol = " "
                      Style = styled px
                      Continuation = false }

                set
                    b
                    clip
                    (px + 1)
                    y
                    { Symbol = " "
                      Style = styled (px + 1)
                      Continuation = false }
            else
                set
                    b
                    clip
                    px
                    y
                    { Symbol = g.Chars
                      Style = styled px
                      Continuation = false }

                if g.Cells = 2 then
                    set
                        b
                        clip
                        (px + 1)
                        y
                        { Symbol = ""
                          Style = styled (px + 1)
                          Continuation = true }

            px <- px + g.Cells

    let fillRect (b: Buffer) (clip: Rect) (area: Rect) style =
        for y in area.Y .. area.Bottom - 1 do
            for x in area.X .. area.Right - 1 do
                // A fill changes colour only, so it must not go through set, which
                // would split a wide character.
                if inside b x y && Rect.contains x y clip then
                    let i = index b x y
                    b.Cells[i] <- { b.Cells[i] with Style = style }

    let toLines (b: Buffer) =
        [ for y in 0 .. b.Height - 1 ->
              System.String.Concat(
                  [ for x in 0 .. b.Width - 1 ->
                        let cell = b.Cells[index b x y]
                        if cell.Continuation then "" else cell.Symbol ]
              ) ]

module Paint =
    open Fansi.Core.Layout

    let private drawBorder (buf: Buffer) (clip: Rect) (rect: Rect) (bc: Border.BorderChars) style =
        if rect.Width > 0 && rect.Height > 0 then
            let middle = String.replicate (max 0 (rect.Width - 2)) (string bc.Horizontal)
            Buffer.writeText buf clip rect.X rect.Y style (string bc.TopLeft + middle + string bc.TopRight)

            for y in rect.Y + 1 .. rect.Bottom - 2 do
                Buffer.set
                    buf
                    clip
                    rect.X
                    y
                    { Symbol = string bc.Vertical
                      Style = style
                      Continuation = false }

                Buffer.set
                    buf
                    clip
                    (rect.Right - 1)
                    y
                    { Symbol = string bc.Vertical
                      Style = style
                      Continuation = false }

            if rect.Height > 1 then
                Buffer.writeText
                    buf
                    clip
                    rect.X
                    (rect.Bottom - 1)
                    style
                    (string bc.BottomLeft + middle + string bc.BottomRight)

    let private writeRow (buf: Buffer) (clip: Rect) x y (row: Run list) =
        row
        |> List.fold
            (fun x run ->
                Buffer.writeText buf clip x y run.Style run.Text
                x + Width.ofString run.Text)
            x
        |> ignore

    /// The title goes into the top border with a space either side, so the box
    /// needs room for both corners, both spaces and at least one character.
    let private drawTitle (buf: Buffer) (clip: Rect) (rect: Rect) (style: Style) (title: Run list) =
        let runs =
            title
            |> List.map (fun r ->
                { r with
                    Style = Runs.cascade style r.Style })
            |> Runs.truncate (rect.Width - 4)

        if rect.Height > 0 && Runs.width runs > 0 then
            Buffer.writeText buf clip (rect.X + 1) rect.Y style " "
            writeRow buf clip (rect.X + 2) rect.Y runs
            Buffer.writeText buf clip (rect.X + 2 + Runs.width runs) rect.Y style " "

    let rec private paint (buf: Buffer) (parent: Style) (ln: LayoutNode) =
        let p = Node.props ln.Node
        let s = Runs.cascade parent (Node.style ln.Node)

        if s.BgColor <> Color.Default then
            Buffer.fillRect buf ln.Clip ln.Rect s

        match Border.chars p.Border with
        | Some bc ->
            drawBorder buf ln.Clip ln.Rect bc s
            drawTitle buf ln.Clip ln.Rect s p.Title
        | None -> ()

        match ln.Node with
        | Text _
        | Line _ ->
            let area = contentRect ln

            // The parent's background is already painted underneath, so a run only
            // carries a background it set itself.
            Runs.wrap (Runs.ofNode { parent with BgColor = Color.Default } ln.Node) area.Width
            |> List.truncate (max 0 area.Height)
            |> List.iteri (fun i row -> writeRow buf ln.Clip area.X (area.Y + i) row)
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
