namespace Fansi

open Fansi.Core

/// Stateless text display component with styling support
[<RequireQualifiedAccess>]
module TextComponent =

    let view (style: Style) (text: string) : Node =
        Node.styledText style text

    let plain (text: string) : Node =
        Node.text text

    let bold (text: string) : Node =
        Node.styledText { Style.Default with Bold = true } text

    let colored (color: Color.Color) (text: string) : Node =
        Node.styledText { Style.Default with FgColor = color } text

    let heading (text: string) : Node =
        Node.styledText { Style.Default with Bold = true; Underline = true } text
