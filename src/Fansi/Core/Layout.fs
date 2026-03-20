namespace Fansi.Core

module Layout =
    type Rect =
        { X: int
          Y: int
          Width: int
          Height: int }

    type Cell =
        { Char: char
          Style: Style }

        static member Empty = { Char = ' '; Style = Style.Default }

    /// A positioned text span ready for rendering
    type Span =
        { X: int
          Y: int
          Text: string
          Style: Style }

    // type ErasedEventHandler =
    //     | ErasedOnKeyPress of (ConsoleKeyInfo -> obj option)
    //     | ErasedOnClick of obj
    //     | ErasedOnMouseDown of obj
    //     | ErasedOnMouseUp of obj
    //     | ErasedOnMouseEnter of obj
    //     | ErasedOnMouseLeave of obj
    //     | ErasedOnFocus of obj
    //     | ErasedOnBlur of obj

    // let inline eraseHandler<'msg> (ev: Fansi.Core.EventHandler<'msg>) : ErasedEventHandler =
    //     match ev with
    //     | Fansi.Core.OnKeyPress handler -> ErasedOnKeyPress(fun ki -> handler ki |> Option.map box)
    //     | Fansi.Core.OnClick msg -> ErasedOnClick(box msg)
    //     | Fansi.Core.OnMouseDown msg -> ErasedOnMouseDown(box msg)
    //     | Fansi.Core.OnMouseUp msg -> ErasedOnMouseUp(box msg)
    //     | Fansi.Core.OnMouseEnter msg -> ErasedOnMouseEnter(box msg)
    //     | Fansi.Core.OnMouseLeave msg -> ErasedOnMouseLeave(box msg)
    //     | Fansi.Core.OnFocus msg -> ErasedOnFocus(box msg)
    //     | Fansi.Core.OnBlur msg -> ErasedOnBlur(box msg)

    /// The resolved layout of a single node, with its absolute position, size, and content
    type LayoutNode =
        { Rect: Rect
          // Events: ErasedEventHandler list
          Content: LayoutContent }

    and LayoutContent =
        | TextContent of string * Style
        | ContainerContent of LayoutNode list

    type LayoutResult =
        { Nodes: LayoutNode list
          Width: int
          Height: int }

    module Border =
        type BorderChars =
            { TopLeft: char
              TopRight: char
              BottomLeft: char
              BottomRight: char
              Horizontal: char
              Vertical: char }

        let getBorderChars style =
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

        let borderThickness style =
            match style with
            | NoBorder -> 0
            | _ -> 1

    let private resolveSize (size: Size option) (available: int) =
        match size with
        | None -> available
        | Some(Fixed n) -> min n available
        | Some(Percent p) -> min (int (float available * p / 100.0)) available

    let private clampSize (minS: int option) (maxS: int option) (value: int) =
        let v =
            match minS with
            | Some m -> max m value
            | None -> value

        match maxS with
        | Some m -> min m v
        | None -> v

    let private measureTextWidth (text: string) =
        let mutable w = 0

        for c in text do
            if c <> '\n' && c <> '\r' then
                w <- w + 1

        w

    let private splitTextLines (text: string) (maxWidth: int) =
        if maxWidth <= 0 then
            [| "" |]
        else
            let lines = text.Split('\n')

            [| for line in lines do
                   if line.Length <= maxWidth then
                       yield line
                   else
                       let mutable pos = 0

                       while pos < line.Length do
                           let len = min maxWidth (line.Length - pos)
                           yield line.Substring(pos, len)
                           pos <- pos + len |]

    /// Measure the intrinsic size of a node (minimum content size)
    let rec private measureNode (node: Node) (availW: int) (availH: int) : int * int =
        match node with
        | Empty -> (0, 0)
        | Text(text, _) ->
            let lines = splitTextLines text availW
            let w = lines |> Array.map (fun l -> l.Length) |> Array.fold max 0
            (w, lines.Length)
        | Box(layout, _, children) ->
            let bt = Border.borderThickness layout.Border

            let innerAvailW =
                availW
                - layout.Padding.Left
                - layout.Padding.Right
                - bt * 2
                - layout.Margin.Left
                - layout.Margin.Right

            let innerAvailH =
                availH
                - layout.Padding.Top
                - layout.Padding.Bottom
                - bt * 2
                - layout.Margin.Top
                - layout.Margin.Bottom

            let childSizes =
                children |> List.map (fun c -> measureNode c innerAvailW innerAvailH)

            let contentW, contentH =
                match layout.Direction with
                | Row ->
                    let w = childSizes |> List.sumBy fst
                    let h = childSizes |> List.map snd |> List.fold max 0
                    (w, h)
                | Column ->
                    let w = childSizes |> List.map fst |> List.fold max 0
                    let h = childSizes |> List.sumBy snd
                    (w, h)

            let totalW =
                contentW
                + layout.Padding.Left
                + layout.Padding.Right
                + bt * 2
                + layout.Margin.Left
                + layout.Margin.Right

            let totalH =
                contentH
                + layout.Padding.Top
                + layout.Padding.Bottom
                + bt * 2
                + layout.Margin.Top
                + layout.Margin.Bottom

            (totalW, totalH)

    /// Perform layout, returning a tree of LayoutNodes with absolute positions
    let rec private layoutNode (node: Node) (x: int) (y: int) (availW: int) (availH: int) : LayoutNode option =
        match node with
        | Empty -> None
        | Text(text, style) ->
            let lines = splitTextLines text availW
            let w = lines |> Array.map _.Length |> Array.fold max 0
            let h = lines.Length
            let displayText = lines |> String.concat "\n"

            Some
                { Rect =
                    { X = x
                      Y = y
                      Width = min w availW
                      Height = min h availH }
                  // Events = []
                  Content = TextContent(displayText, style) }
        | Box(layout, _, children) ->
            let bt = Border.borderThickness layout.Border
            let marginX = x + layout.Margin.Left
            let marginY = y + layout.Margin.Top

            let outerW =
                resolveSize layout.Width availW |> clampSize layout.MinWidth layout.MaxWidth

            let outerH =
                resolveSize layout.Height availH |> clampSize layout.MinHeight layout.MaxHeight

            let boxW = outerW - layout.Margin.Left - layout.Margin.Right |> max 0
            let boxH = outerH - layout.Margin.Top - layout.Margin.Bottom |> max 0
            let innerX = marginX + bt + layout.Padding.Left
            let innerY = marginY + bt + layout.Padding.Top
            let innerW = boxW - bt * 2 - layout.Padding.Left - layout.Padding.Right |> max 0
            let innerH = boxH - bt * 2 - layout.Padding.Top - layout.Padding.Bottom |> max 0

            // let erasedEvents = events |> List.map eraseHandler

            let childNodes = layoutChildren layout children innerX innerY innerW innerH

            Some
                { Rect =
                    { X = marginX
                      Y = marginY
                      Width = boxW
                      Height = boxH }
                  // Events = erasedEvents
                  Content = ContainerContent childNodes }

    and private layoutChildren
        (layout: LayoutProps)
        (children: Node list)
        (startX: int)
        (startY: int)
        (availW: int)
        (availH: int)
        : LayoutNode list =
        if children.IsEmpty then
            []
        else
            let childMeasures =
                children
                |> List.map (fun c ->
                    let mw, mh = measureNode c availW availH
                    c, mw, mh)

            match layout.Direction with
            | Row -> layoutRow layout childMeasures startX startY availW availH
            | Column -> layoutColumn layout childMeasures startX startY availW availH

    and private resolveChildWidth (child: Node) (natW: int) (availW: int) =
        match child with
        | Box(l, _, _) ->
            match l.Width with
            | Some s -> resolveSize (Some s) availW |> clampSize l.MinWidth l.MaxWidth
            | None -> natW
        | _ -> natW

    and private resolveChildHeight (child: Node) (natH: int) (availH: int) =
        match child with
        | Box(l, _, _) ->
            match l.Height with
            | Some s -> resolveSize (Some s) availH |> clampSize l.MinHeight l.MaxHeight
            | None -> natH
        | _ -> natH

    and private clearExplicitWidth (child: Node) =
        match child with
        | Box(l, s, c) when l.Width.IsSome -> Box({ l with Width = None }, s, c)
        | other -> other

    and private clearExplicitHeight (child: Node) =
        match child with
        | Box(l, s, c) when l.Height.IsSome -> Box({ l with Height = None }, s, c)
        | other -> other

    and private layoutRow
        (layout: LayoutProps)
        (childMeasures: (Node * int * int) list)
        (startX: int)
        (startY: int)
        (availW: int)
        (availH: int)
        : LayoutNode list =
        let resolvedWidths =
            childMeasures
            |> List.map (fun (c, natW, _) -> resolveChildWidth c natW availW)

        let totalNatural = resolvedWidths |> List.sum
        let freeSpace = max 0 (availW - totalNatural)

        let totalGrow =
            childMeasures
            |> List.sumBy (fun (c, _, _) ->
                match c with
                | Box(l, _, _) -> l.Grow
                | _ -> 0.0)

        let childWidths =
            List.zip childMeasures resolvedWidths
            |> List.map (fun ((c, _, _), resW) ->
                let grow =
                    match c with
                    | Box(l, _, _) -> l.Grow
                    | _ -> 0.0

                if totalGrow > 0.0 && grow > 0.0 then
                    resW + int (float freeSpace * grow / totalGrow)
                else
                    resW)

        let totalUsed = childWidths |> List.sum
        let remaining = max 0 (availW - totalUsed)

        let offsets =
            match layout.Justify with
            | JustifyStart ->
                childWidths
                |> List.scan (fun acc w -> acc + w) 0
                |> List.take childWidths.Length
            | JustifyEnd ->
                childWidths
                |> List.scan (fun acc w -> acc + w) remaining
                |> List.take childWidths.Length
            | JustifyCenter ->
                childWidths
                |> List.scan (fun acc w -> acc + w) (remaining / 2)
                |> List.take childWidths.Length
            | SpaceBetween ->
                let gaps = max 1 (childWidths.Length - 1)
                let gap = if childWidths.Length > 1 then remaining / gaps else 0

                childWidths
                |> List.mapi (fun i _ -> i)
                |> List.scan (fun acc i -> acc + childWidths[i] + (if i > 0 then gap else 0)) 0
                |> List.tail
                |> List.mapi (fun i offset -> offset - childWidths[i])
            | SpaceAround ->
                let gap = remaining / (max 1 childWidths.Length)

                childWidths
                |> List.scan (fun acc w -> acc + w + gap) (gap / 2)
                |> List.take childWidths.Length

        List.zip3 childMeasures childWidths offsets
        |> List.choose (fun ((child, _, natH), w, offsetX) ->
            let childH =
                match layout.Align with
                | AlignStretch -> availH
                | _ -> min natH availH

            let childY =
                match layout.Align with
                | AlignStart
                | AlignStretch -> startY
                | AlignEnd -> startY + availH - childH
                | AlignCenter -> startY + (availH - childH) / 2

            layoutNode (clearExplicitWidth child) (startX + offsetX) childY (min w availW) childH)

    and private layoutColumn
        (layout: LayoutProps)
        (childMeasures: (Node * int * int) list)
        (startX: int)
        (startY: int)
        (availW: int)
        (availH: int)
        : LayoutNode list =
        let resolvedHeights =
            childMeasures
            |> List.map (fun (c, _, natH) -> resolveChildHeight c natH availH)

        let totalNatural = resolvedHeights |> List.sum
        let freeSpace = max 0 (availH - totalNatural)

        let totalGrow =
            childMeasures
            |> List.sumBy (fun (c, _, _) ->
                match c with
                | Box(l, _, _) -> l.Grow
                | _ -> 0.0)

        let childHeights =
            List.zip childMeasures resolvedHeights
            |> List.map (fun ((c, _, _), resH) ->
                let grow =
                    match c with
                    | Box(l, _, _) -> l.Grow
                    | _ -> 0.0

                if totalGrow > 0.0 && grow > 0.0 then
                    resH + int (float freeSpace * grow / totalGrow)
                else
                    resH)

        let totalUsed = childHeights |> List.sum
        let remaining = max 0 (availH - totalUsed)

        let offsets =
            match layout.Justify with
            | JustifyStart ->
                childHeights
                |> List.scan (fun acc h -> acc + h) 0
                |> List.take childHeights.Length
            | JustifyEnd ->
                childHeights
                |> List.scan (fun acc h -> acc + h) remaining
                |> List.take childHeights.Length
            | JustifyCenter ->
                childHeights
                |> List.scan (fun acc h -> acc + h) (remaining / 2)
                |> List.take childHeights.Length
            | SpaceBetween ->
                let gaps = max 1 (childHeights.Length - 1)
                let gap = if childHeights.Length > 1 then remaining / gaps else 0

                childHeights
                |> List.mapi (fun i _ -> i)
                |> List.scan (fun acc i -> acc + childHeights[i] + (if i > 0 then gap else 0)) 0
                |> List.tail
                |> List.mapi (fun i offset -> offset - childHeights[i])
            | SpaceAround ->
                let gap = remaining / (max 1 childHeights.Length)

                childHeights
                |> List.scan (fun acc h -> acc + h + gap) (gap / 2)
                |> List.take childHeights.Length

        List.zip3 childMeasures childHeights offsets
        |> List.choose (fun ((child, natW, _), h, offsetY) ->
            let childW =
                match layout.Align with
                | AlignStretch -> availW
                | _ -> min (resolveChildWidth child natW availW) availW

            let childX =
                match layout.Align with
                | AlignStart
                | AlignStretch -> startX
                | AlignEnd -> startX + availW - childW
                | AlignCenter -> startX + (availW - childW) / 2

            layoutNode (clearExplicitHeight child) childX (startY + offsetY) childW (min h availH))

    /// Flatten the layout tree into a list of positioned spans for rendering
    let rec flattenToSpans (node: LayoutNode) : Span list =
        match node.Content with
        | TextContent(text, style) ->
            let lines = text.Split('\n')

            [ for i in 0 .. lines.Length - 1 do
                  if node.Rect.Y + i >= 0 then
                      let line =
                          if lines[i].Length > node.Rect.Width then
                              lines[i].Substring(0, node.Rect.Width)
                          else
                              lines[i]

                      yield
                          { X = node.Rect.X
                            Y = node.Rect.Y + i
                            Text = line
                            Style = style } ]
        | ContainerContent children -> children |> List.collect flattenToSpans

    /// Render border spans for a box node
    let borderSpans (rect: Rect) (borderStyle: BorderStyle) (style: Style) : Span list =
        match Border.getBorderChars borderStyle with
        | None -> []
        | Some bc ->
            let topLine =
                string bc.TopLeft
                + String.replicate (max 0 (rect.Width - 2)) (string bc.Horizontal)
                + string bc.TopRight

            let bottomLine =
                string bc.BottomLeft
                + String.replicate (max 0 (rect.Width - 2)) (string bc.Horizontal)
                + string bc.BottomRight

            [ yield
                  { X = rect.X
                    Y = rect.Y
                    Text = topLine
                    Style = style }
              for row in 1 .. rect.Height - 2 do
                  yield
                      { X = rect.X
                        Y = rect.Y + row
                        Text = string bc.Vertical
                        Style = style }

                  yield
                      { X = rect.X + rect.Width - 1
                        Y = rect.Y + row
                        Text = string bc.Vertical
                        Style = style }
              yield
                  { X = rect.X
                    Y = rect.Y + rect.Height - 1
                    Text = bottomLine
                    Style = style } ]

    /// Collect border spans from the layout tree
    let rec collectBorderSpans (node: Node) (layoutNode: LayoutNode) : Span list =
        match node with
        | Box(layout, style, children) ->
            let myBorders = borderSpans layoutNode.Rect layout.Border style

            let childBorders =
                match layoutNode.Content with
                | ContainerContent childLayouts ->
                    List.zip children childLayouts
                    |> List.collect (fun (cn, cl) -> collectBorderSpans cn cl)
                | _ -> []

            myBorders @ childBorders
        | _ -> []

    /// Main entry point: layout a node tree within the given terminal dimensions
    let layout (termWidth: int) (termHeight: int) (root: Node) : LayoutResult =
        let nodes =
            match layoutNode root 0 0 termWidth termHeight with
            | Some n -> [ n ]
            | None -> []

        { Nodes = nodes
          Width = termWidth
          Height = termHeight }

/// Collect all layout nodes with their event handlers for hit-testing
// let rec collectEventNodes (node: LayoutNode) : (Rect * ErasedEventHandler list) list =
//     let mine =
//         if node.Events.IsEmpty then []
//         else [ (node.Rect, node.Events) ]
//     let children =
//         match node.Content with
//         | ContainerContent kids -> kids |> List.collect collectEventNodes
//         | _ -> []
//     mine @ children
