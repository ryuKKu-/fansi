namespace Fansi.Core

[<RequireQualifiedAccess>]
module Color =
    type Color =
        | Default
        | Black
        | Red
        | Green
        | Yellow
        | Blue
        | Magenta
        | Cyan
        | White
        | BrightBlack
        | BrightRed
        | BrightGreen
        | BrightYellow
        | BrightBlue
        | BrightMagenta
        | BrightCyan
        | BrightWhite
        | Rgb of r: int * g: int * b: int

type Style =
    { FgColor: Color.Color
      BgColor: Color.Color
      Bold: bool
      Italic: bool
      Underline: bool
      Strikethrough: bool }

    static member Default =
        { FgColor = Color.Default
          BgColor = Color.Default
          Bold = false
          Italic = false
          Underline = false
          Strikethrough = false }

type Size =
    | Fixed of int
    | Percent of float

type Edges =
    { Top: int
      Right: int
      Bottom: int
      Left: int }

    static member Zero = { Top = 0; Right = 0; Bottom = 0; Left = 0 }

    static member All n = { Top = n; Right = n; Bottom = n; Left = n }

    static member Horizontal n = { Top = 0; Right = n; Bottom = 0; Left = n }

    static member Vertical n = { Top = n; Right = 0; Bottom = n; Left = 0 }

type FlexDirection =
    | Row
    | Column

type FlexJustify =
    | JustifyStart
    | JustifyEnd
    | JustifyCenter
    | SpaceBetween
    | SpaceAround

type FlexAlign =
    | AlignStart
    | AlignEnd
    | AlignCenter
    | AlignStretch

type FlexWrap =
    | NoWrap
    | Wrap

type BorderStyle =
    | NoBorder
    | Single
    | Double
    | Rounded
    | Heavy
    | Ascii

type LayoutProps =
    { Direction: FlexDirection
      Justify: FlexJustify
      Align: FlexAlign
      Wrap: FlexWrap
      Width: Size option
      Height: Size option
      MinWidth: int option
      MinHeight: int option
      MaxWidth: int option
      MaxHeight: int option
      Padding: Edges
      Margin: Edges
      Grow: float
      Shrink: float
      Border: BorderStyle }

    static member Default =
        { Direction = Column
          Justify = JustifyStart
          Align = AlignStart
          Wrap = NoWrap
          Width = None
          Height = None
          MinWidth = None
          MinHeight = None
          MaxWidth = None
          MaxHeight = None
          Padding = Edges.Zero
          Margin = Edges.Zero
          Grow = 0.0
          Shrink = 1.0
          Border = NoBorder }

type Node =
    | Text of text: string * style: Style
    | Box of layout: LayoutProps * style: Style * children: Node list
    | Empty

module Node =
    let text str = Text(str, Style.Default)

    let styledText style str = Text(str, style)

    let box children = Box(LayoutProps.Default, Style.Default, children)

    let styledBox layout style children = Box(layout, style, children)

    let row children =
        Box({ LayoutProps.Default with Direction = Row }, Style.Default, children)

    let column children =
        Box({ LayoutProps.Default with Direction = Column }, Style.Default, children)

    let empty = Empty

    let withStyle style node =
        match node with
        | Text(t, _) -> Text(t, style)
        | Box(layout, _, children) -> Box(layout, style, children)
        | Empty -> Empty

    let withLayout layout node =
        match node with
        | Box(_, style, children) -> Box(layout, style, children)
        | other -> other

    // --- Split-pane helpers ---

    let private wrapWithGrow grow child =
        match child with
        | Box(l, s, c) -> Box({ l with Grow = max l.Grow grow }, s, c)
        | other -> Box({ LayoutProps.Default with Grow = grow }, Style.Default, [ other ])

    let splitColumns children =
        Box({ LayoutProps.Default with Direction = Row }, Style.Default, children |> List.map (wrapWithGrow 1.0))

    let splitRows children =
        Box(LayoutProps.Default, Style.Default, children |> List.map (wrapWithGrow 1.0))

    let splitColumnsRatio (items: (int * Node) list) =
        let wrap (ratio, child) = wrapWithGrow (float ratio) child
        Box({ LayoutProps.Default with Direction = Row }, Style.Default, items |> List.map wrap)

    let splitRowsRatio (items: (int * Node) list) =
        let wrap (ratio, child) = wrapWithGrow (float ratio) child
        Box(LayoutProps.Default, Style.Default, items |> List.map wrap)

    // --- Grid helper ---

    let grid (rows: Node list list) =
        let makeRow cells =
            Box({ LayoutProps.Default with Direction = Row }, Style.Default, cells |> List.map (wrapWithGrow 1.0))

        Box(LayoutProps.Default, Style.Default, rows |> List.map makeRow)

    // --- Modifier helpers ---

    let withWidth size node =
        match node with
        | Box(l, s, c) -> Box({ l with Width = Some size }, s, c)
        | other -> other

    let withHeight size node =
        match node with
        | Box(l, s, c) -> Box({ l with Height = Some size }, s, c)
        | other -> other

    let withGrow grow node =
        match node with
        | Box(l, s, c) -> Box({ l with Grow = grow }, s, c)
        | other -> other

    let withPadding padding node =
        match node with
        | Box(l, s, c) -> Box({ l with Padding = padding }, s, c)
        | other -> other

    let withMargin margin node =
        match node with
        | Box(l, s, c) -> Box({ l with Margin = margin }, s, c)
        | other -> other

    let withBorder border node =
        match node with
        | Box(l, s, c) -> Box({ l with Border = border }, s, c)
        | other -> other
