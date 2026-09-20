namespace Fansi.Core

module Layout =

    module Border =
        type BorderChars =
            { TopLeft: char
              TopRight: char
              BottomLeft: char
              BottomRight: char
              Horizontal: char
              Vertical: char }

        let chars style =
            match style with
            | NoBorder -> None
            | Single ->
                Some
                    { TopLeft = '┌'
                      TopRight = '┐'
                      BottomLeft = '└'
                      BottomRight = '┘'
                      Horizontal = '─'
                      Vertical = '│' }
            | Double ->
                Some
                    { TopLeft = '╔'
                      TopRight = '╗'
                      BottomLeft = '╚'
                      BottomRight = '╝'
                      Horizontal = '═'
                      Vertical = '║' }
            | Rounded ->
                Some
                    { TopLeft = '╭'
                      TopRight = '╮'
                      BottomLeft = '╰'
                      BottomRight = '╯'
                      Horizontal = '─'
                      Vertical = '│' }
            | Heavy ->
                Some
                    { TopLeft = '┏'
                      TopRight = '┓'
                      BottomLeft = '┗'
                      BottomRight = '┛'
                      Horizontal = '━'
                      Vertical = '┃' }
            | Ascii ->
                Some
                    { TopLeft = '+'
                      TopRight = '+'
                      BottomLeft = '+'
                      BottomRight = '+'
                      Horizontal = '-'
                      Vertical = '|' }

        let thickness style =
            match chars style with
            | None -> 0
            | Some _ -> 1

    /// Break text into display lines. Explicit newlines always break; anything
    /// longer than maxWidth is cut hard, with no word awareness.
    let wrapText (text: string) (maxWidth: int) : string list =
        if maxWidth <= 0 then
            []
        else
            [ for line in text.Split('\n') do
                  if line.Length <= maxWidth then
                      yield line
                  else
                      let mutable pos = 0

                      while pos < line.Length do
                          let take = min maxWidth (line.Length - pos)
                          yield line.Substring(pos, take)
                          pos <- pos + take ]

    let private chrome (p: Props) =
        let bt = Border.thickness p.Border

        { Top = p.Margin.Top + bt + p.Padding.Top
          Right = p.Margin.Right + bt + p.Padding.Right
          Bottom = p.Margin.Bottom + bt + p.Padding.Bottom
          Left = p.Margin.Left + bt + p.Padding.Left }

    /// The intrinsic outer size of a node: how big it wants to be, including its
    /// own margin, border and padding.
    let rec measure (node: Node) (availW: int) (availH: int) : int * int =
        let p = Node.props node
        let c = chrome p
        let innerW = max 0 (availW - c.Horizontal)
        let innerH = max 0 (availH - c.Vertical)

        let contentW, contentH =
            match node with
            | Text(t, _, _) ->
                let lines = wrapText t innerW
                let w = lines |> List.fold (fun acc (l: string) -> max acc l.Length) 0
                w, List.length lines
            | Container([], _, _) -> 0, 0
            | Container(children, _, _) ->
                let mainAvail, crossAvail =
                    match p.Direction with
                    | Row -> innerW, innerH
                    | Column -> innerH, innerW

                let measured =
                    children
                    |> List.map (fun child ->
                        match p.Direction with
                        | Row -> measure child mainAvail crossAvail
                        | Column ->
                            let w, h = measure child crossAvail mainAvail
                            h, w)

                let mainIntrinsics = measured |> List.map fst
                let crossIntrinsics = measured |> List.map snd
                let constraints = children |> List.map (fun c -> (Node.props c).Main)
                let mainTotal = Solver.solve constraints mainIntrinsics mainAvail |> List.sum
                let crossMax = crossIntrinsics |> List.fold max 0

                match p.Direction with
                | Row -> mainTotal, crossMax
                | Column -> crossMax, mainTotal

        contentW + c.Horizontal, contentH + c.Vertical

    type LayoutNode =
        { Rect: Rect
          Clip: Rect
          Node: Node
          Children: LayoutNode list }

    /// The area inside a node's border and padding, where its content goes.
    let contentRect (ln: LayoutNode) =
        let p = Node.props ln.Node

        ln.Rect |> Rect.deflateBy (Border.thickness p.Border) |> Rect.deflate p.Padding

    /// Where each child starts along the main axis, given the slack left over.
    let private offsets (justify: Justify) (slack: int) (sizes: int list) =
        let count = List.length sizes
        let slack = max 0 slack

        let start =
            match justify with
            | Justify.Start
            | Justify.Between -> 0
            | Justify.End -> slack
            | Justify.Center -> slack / 2

        let gap =
            match justify with
            | Justify.Between when count > 1 -> slack / (count - 1)
            | _ -> 0

        sizes
        |> List.scan (fun acc size -> acc + size + gap) start
        |> List.truncate count

    let private crossSize (align: Align) (child: Node) (intrinsic: int) (available: int) =
        match (Node.props child).Cross with
        | Auto ->
            match align with
            | Align.Stretch -> available
            | _ -> min intrinsic available
        | c -> Solver.solve [ c ] [ intrinsic ] available |> List.head

    let private crossOffset (align: Align) (available: int) (size: int) =
        match align with
        | Align.Start
        | Align.Stretch -> 0
        | Align.End -> max 0 (available - size)
        | Align.Center -> max 0 (available - size) / 2

    /// Place a node and its descendants inside `slot`, clipped to `clip`.
    let rec private place (node: Node) (slot: Rect) (clip: Rect) : LayoutNode =
        let p = Node.props node
        let box = Rect.deflate p.Margin slot
        let boxClip = Rect.intersect clip box

        let inner =
            box |> Rect.deflateBy (Border.thickness p.Border) |> Rect.deflate p.Padding

        let innerClip = Rect.intersect boxClip inner

        let children =
            match node with
            | Text _ -> []
            | Container([], _, _) -> []
            | Container(kids, _, _) ->
                let mainAvail, crossAvail =
                    match p.Direction with
                    | Row -> inner.Width, inner.Height
                    | Column -> inner.Height, inner.Width

                let measured =
                    kids
                    |> List.map (fun kid ->
                        match p.Direction with
                        | Row -> measure kid mainAvail crossAvail
                        | Column ->
                            let w, h = measure kid crossAvail mainAvail
                            h, w)

                let constraints = kids |> List.map (fun k -> (Node.props k).Main)
                let sizes = Solver.solve constraints (List.map fst measured) mainAvail
                let slack = mainAvail - List.sum sizes
                let starts = offsets p.Justify slack sizes

                List.zip3 kids (List.zip sizes starts) measured
                |> List.map (fun (kid, (size, start), (_, crossIntrinsic)) ->
                    let cs = crossSize p.Align kid crossIntrinsic crossAvail
                    let co = crossOffset p.Align crossAvail cs

                    let slot =
                        match p.Direction with
                        | Row ->
                            { X = inner.X + start
                              Y = inner.Y + co
                              Width = size
                              Height = cs }
                        | Column ->
                            { X = inner.X + co
                              Y = inner.Y + start
                              Width = cs
                              Height = size }

                    place kid slot innerClip)

        { Rect = box
          Clip = boxClip
          Node = node
          Children = children }

    /// Lay out a tree inside the given area.
    let arrange (node: Node) (area: Rect) : LayoutNode = place node area area
