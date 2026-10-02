namespace Fansi

open Fansi.Core

/// Constructors and modifiers for building a view. Every modifier is
/// Node -> Node, so they compose with |> and apply to a node built anywhere.
[<RequireQualifiedAccess>]
module Ui =

    // --- constructors ---

    let text s = Text(s, Style.Default, Props.Default)

    let col children =
        Container(
            children,
            Style.Default,
            { Props.Default with
                Direction = Column }
        )

    let row children =
        Container(children, Style.Default, { Props.Default with Direction = Row })

    let empty = Container([], Style.Default, Props.Default)

    let line children =
        Line(children, Style.Default, Props.Default)

    // --- claim along the parent's axis ---

    let len n =
        Node.mapProps (fun p -> { p with Main = Len n })

    let pct n =
        Node.mapProps (fun p -> { p with Main = Pct n })

    let ratio a b =
        Node.mapProps (fun p -> { p with Main = Ratio(a, b) })

    let fill weight =
        Node.mapProps (fun p -> { p with Main = Fill weight })

    let minLen n =
        Node.mapProps (fun p -> { p with Main = Min n })

    let maxLen n =
        Node.mapProps (fun p -> { p with Main = Max n })

    let auto node =
        Node.mapProps (fun p -> { p with Main = Auto }) node

    // --- claim across the parent's axis ---

    let cross c =
        Node.mapProps (fun p -> { p with Cross = c })

    // A negative edge grows the content area back over the border, or pushes the
    // node outside the space its parent gave it.
    let private atLeastZero (e: Edges) =
        { Top = max 0 e.Top
          Right = max 0 e.Right
          Bottom = max 0 e.Bottom
          Left = max 0 e.Left }

    // --- container ---

    let title (text: string) =
        Node.mapProps (fun p ->
            { p with
                Title = [ { Text = text; Style = Style.Default } ] })

    let titleWith (node: Node) =
        Node.mapProps (fun p ->
            { p with
                Title = Runs.ofNode Style.Default node })

    let direction d =
        Node.mapProps (fun p -> { p with Direction = d })

    let justify j =
        Node.mapProps (fun p -> { p with Justify = j })

    let align a =
        Node.mapProps (fun p -> { p with Align = a })

    let border b =
        Node.mapProps (fun p -> { p with Border = b })

    let padding edges =
        Node.mapProps (fun p -> { p with Padding = atLeastZero edges })

    let pad n = padding (Edges.All n)
    let padX n = padding (Edges.X n)
    let padY n = padding (Edges.Y n)

    let margins edges =
        Node.mapProps (fun p -> { p with Margin = atLeastZero edges })

    let margin n = margins (Edges.All n)
    let marginX n = margins (Edges.X n)
    let marginY n = margins (Edges.Y n)

    // --- style ---

    let style s = Node.withStyle s

    let fg c =
        Node.mapStyle (fun s -> { s with FgColor = c })

    let bg c =
        Node.mapStyle (fun s -> { s with BgColor = c })

    let bold node =
        Node.mapStyle (fun s -> { s with Bold = true }) node

    let italic node =
        Node.mapStyle (fun s -> { s with Italic = true }) node

    let underline node =
        Node.mapStyle (fun s -> { s with Underline = true }) node

    let strike node =
        Node.mapStyle (fun s -> { s with Strikethrough = true }) node
