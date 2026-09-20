module Fansi.Tests.UiTests

open Xunit
open Fansi.Core
open Fansi

[<Fact>]
let ``text carries default style and props`` () =
    match Ui.text "hello" with
    | Text(t, s, p) ->
        Assert.Equal("hello", t)
        Assert.Equal(Style.Default, s)
        Assert.Equal(Props.Default, p)
    | other -> failwith $"expected Text, got {other}"

[<Fact>]
let ``row and col differ only in direction`` () =
    Assert.Equal(Row, (Ui.row [] |> Node.props).Direction)
    Assert.Equal(Column, (Ui.col [] |> Node.props).Direction)

[<Fact>]
let ``modifiers apply to text as well as containers`` () =
    // the old API silently ignored layout changes on a Text node
    let node = Ui.text "x" |> Ui.fill 1
    Assert.Equal(Fill 1, (Node.props node).Main)

[<Fact>]
let ``modifiers compose in a pipeline`` () =
    let node =
        Ui.col [ Ui.text "a" ]
        |> Ui.len 30
        |> Ui.border Rounded
        |> Ui.padX 1
        |> Ui.fg Color.Cyan

    let p = Node.props node
    Assert.Equal(Len 30, p.Main)
    Assert.Equal(Rounded, p.Border)
    Assert.Equal(Edges.X 1, p.Padding)
    Assert.Equal(Color.Cyan, (Node.style node).FgColor)

[<Fact>]
let ``a later modifier wins over an earlier one`` () =
    let node = Ui.text "x" |> Ui.len 10 |> Ui.len 20
    Assert.Equal(Len 20, (Node.props node).Main)

[<Fact>]
let ``a modifier can be applied to a node built elsewhere`` () =
    // this is the case attribute lists cannot express without an extra wrapper
    let fromComponent = Ui.col [ Ui.text "built by someone else" ]
    let decorated = fromComponent |> Ui.fill 2
    Assert.Equal(Fill 2, (Node.props decorated).Main)
    Assert.Equal(1, List.length (Node.children decorated))

[<Fact>]
let ``modifiers can be composed into a reusable function`` () =
    let card = Ui.border Rounded >> Ui.padX 1 >> Ui.fill 1
    let p = Ui.col [] |> card |> Node.props
    Assert.Equal(Rounded, p.Border)
    Assert.Equal(Edges.X 1, p.Padding)
    Assert.Equal(Fill 1, p.Main)

[<Fact>]
let ``style modifiers set their own attribute and leave the rest alone`` () =
    let s = Ui.text "x" |> Ui.bold |> Ui.underline |> Node.style
    Assert.True(s.Bold)
    Assert.True(s.Underline)
    Assert.False(s.Italic)

[<Fact>]
let ``empty is a container with no children`` () =
    Assert.Equal<Node list>([], Node.children Ui.empty)
