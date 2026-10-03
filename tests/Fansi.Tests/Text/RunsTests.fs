module Fansi.Tests.RunsTests

open Xunit
open Fansi
open FsCheck.Xunit
open Fansi.Core

let private plain text = { Text = text; Style = Style.Default }

let private texts (rows: Run list list) =
    rows
    |> List.map (fun row -> row |> List.map (fun r -> r.Text) |> String.concat "")

[<Property>]
let ``rows fit the width and keep every character that can fit`` (text: string) (width: int) =
    let text = if isNull text then "" else text
    let maxWidth = abs (width % 20) + 1
    let rows = Runs.wrap [ plain text ] maxWidth

    let kept =
        text.Split('\n')
        |> Array.map (fun segment ->
            Width.glyphs segment
            |> List.filter (fun g -> g.Cells <= maxWidth)
            |> List.map (fun g -> g.Chars)
            |> String.concat "")
        |> String.concat ""

    List.forall (fun row -> Runs.width row <= maxWidth) rows
    && String.concat "" (texts rows) = kept

[<Fact>]
let ``a wide character that does not fit moves to the next row`` () =
    Assert.Equal<string list>([ "a"; "字" ], texts (Runs.wrap [ plain "a字" ] 2))

[<Fact>]
let ``a character wider than the row is dropped`` () =
    Assert.Equal<string list>([ "a"; "b" ], texts (Runs.wrap [ plain "a字b" ] 1))

[<Fact>]
let ``a mark stays with its character across a break`` () =
    Assert.Equal<string list>([ "ab́"; "c" ], texts (Runs.wrap [ plain "ab́c" ] 2))

[<Fact>]
let ``a newline inside a run starts a row`` () =
    Assert.Equal<string list>([ "ab"; ""; "c" ], texts (Runs.wrap [ plain "ab\n\nc" ] 10))

[<Fact>]
let ``a line measures like its text`` () =
    Assert.Equal((4, 1), Layout.measure (Ui.line [ Ui.text "ab"; Ui.text "字" ]) 80 24)

[<Fact>]
let ``an empty line measures like empty text`` () =
    Assert.Equal(Layout.measure (Ui.text "") 80 24, Layout.measure (Ui.line []) 80 24)
