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

        // Margin and padding are unconstrained ints from user code, so a wrapped
        // negative would make the inner area larger than the outer one.
        let side margin padding = Saturating.add3 margin bt padding

        { Top = side p.Margin.Top p.Padding.Top
          Right = side p.Margin.Right p.Padding.Right
          Bottom = side p.Margin.Bottom p.Padding.Bottom
          Left = side p.Margin.Left p.Padding.Left }

    /// How much room a child gets across its parent's axis, worked out without
    /// its intrinsic size. Only Len, Pct and Ratio can be settled that way; the
    /// rest need the intrinsic, so they keep the whole available extent. Measuring
    /// a child against this instead of the parent's full cross extent is what stops
    /// it being measured at one width and then placed at another.
    let private crossExtent (c: Constraint) (available: int) =
        match c with
        | Len _
        | Pct _
        | Ratio _ -> Solver.solve [ c ] [ 0 ] available |> List.head
        | Auto
        | Fill _
        | Min _
        | Max _ -> available

    /// The intrinsic outer size of a node: how big it wants to be, including its
    /// own margin, border and padding.
    let rec measure (node: Node) (availW: int) (availH: int) : int * int =
        let p = Node.props node
        let c = chrome p
        let innerW = max 0 (Saturating.sub availW c.Horizontal)
        let innerH = max 0 (Saturating.sub availH c.Vertical)

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

                let measured = measureChildren p.Direction children mainAvail crossAvail

                let mainIntrinsics = measured |> List.map fst
                let crossIntrinsics = measured |> List.map snd
                let constraints = children |> List.map (fun c -> (Node.props c).Main)
                let mainTotal = Solver.solve constraints mainIntrinsics mainAvail |> List.sum
                let crossMax = crossIntrinsics |> List.fold max 0

                match p.Direction with
                | Row -> mainTotal, crossMax
                | Column -> crossMax, mainTotal

        // Saturated for the same reason as chrome: the chrome itself can reach
        // Int32.MaxValue, so adding content to it wraps and hands the solver a
        // negative intrinsic size. Unlike an edge sum, a measured size is never
        // negative, so this also floors the result at zero.
        let outer inner edge = max 0 (Saturating.add inner edge)

        outer contentW c.Horizontal, outer contentH c.Vertical

    /// Measure children as (main, cross) pairs in the parent's own axes.
    and private measureChildren direction (children: Node list) mainAvail crossAvail =
        children
        |> List.map (fun child ->
            let cross = crossExtent (Node.props child).Cross crossAvail

            match direction with
            | Row -> measure child mainAvail cross
            | Column ->
                let w, h = measure child cross mainAvail
                h, w)

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

        if count = 0 then
            []
        else
            let start =
                match justify with
                | Justify.Start
                | Justify.Between -> 0
                | Justify.End -> slack
                | Justify.Center -> slack / 2

            // Between spreads the slack across the count-1 gaps. One uniform
            // slack/(count-1) truncates and leaves the last child short of the far
            // edge, so the gaps are shared out the same way child sizes are.
            let gaps =
                match justify with
                | Justify.Between when count > 1 -> Solver.distribute slack (List.replicate (count - 1) 1)
                | _ -> List.replicate (count - 1) 0

            List.map2 (+) sizes (gaps @ [ 0 ]) |> List.scan (+) start |> List.truncate count

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

                let measured = measureChildren p.Direction kids mainAvail crossAvail

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
