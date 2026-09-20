namespace Fansi.Core

type Node =
    | Text of text: string * style: Style * props: Props
    | Container of children: Node list * style: Style * props: Props

module Node =
    let props node =
        match node with
        | Text(_, _, p) -> p
        | Container(_, _, p) -> p

    let style node =
        match node with
        | Text(_, s, _) -> s
        | Container(_, s, _) -> s

    let children node =
        match node with
        | Text _ -> []
        | Container(c, _, _) -> c

    let withProps p node =
        match node with
        | Text(t, s, _) -> Text(t, s, p)
        | Container(c, s, _) -> Container(c, s, p)

    let withStyle s node =
        match node with
        | Text(t, _, p) -> Text(t, s, p)
        | Container(c, _, p) -> Container(c, s, p)

    let mapProps f node = withProps (f (props node)) node

    let mapStyle f node = withStyle (f (style node)) node
