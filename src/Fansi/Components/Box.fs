namespace Fansi

open Fansi.Core

/// Container component with flexbox layout, borders, and styling
[<RequireQualifiedAccess>]
module BoxComponent =

    let view (layout: LayoutProps) (style: Style) (children: Node list) : Node =
        Box(layout, style, children)

    let bordered (border: BorderStyle) (children: Node list) : Node =
        Box({ LayoutProps.Default with Border = border }, Style.Default, children)

    let row (children: Node list) : Node =
        Box({ LayoutProps.Default with Direction = Row }, Style.Default, children)

    let column (children: Node list) : Node =
        Box({ LayoutProps.Default with Direction = Column }, Style.Default, children)

    let padded (padding: int) (children: Node list) : Node =
        Box({ LayoutProps.Default with Padding = Edges.All padding }, Style.Default, children)

    let titled (title: string) (border: BorderStyle) (style: Style) (children: Node list) : Node =
        Box(
            { LayoutProps.Default with Border = border; Padding = { Edges.Zero with Top = 1 } },
            style,
            Node.styledText { Style.Default with Bold = true } title :: children
        )
