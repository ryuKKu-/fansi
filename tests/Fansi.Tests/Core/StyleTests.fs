module Fansi.Tests.StyleTests

open Xunit
open Fansi.Core

[<Fact>]
let ``default style has no colour and no attributes`` () =
    let s = Style.Default
    Assert.Equal(Color.Default, s.FgColor)
    Assert.Equal(Color.Default, s.BgColor)
    Assert.False(s.Bold)
    Assert.False(s.Italic)
    Assert.False(s.Underline)
    Assert.False(s.Strikethrough)

[<Fact>]
let ``styles compare structurally`` () =
    let a =
        { Style.Default with
            FgColor = Color.Cyan }

    let b =
        { Style.Default with
            FgColor = Color.Cyan }

    Assert.Equal(a, b)
    Assert.NotEqual(a, Style.Default)
