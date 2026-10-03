module Fansi.Tests.WidthTests

open Xunit
open Fansi.Core

[<Fact>]
let ``ascii is one cell`` () = Assert.Equal(1, Width.ofString "a")

[<Fact>]
let ``a CJK character is two cells`` () = Assert.Equal(2, Width.ofString "字")

[<Fact>]
let ``a combining mark adds nothing`` () = Assert.Equal(1, Width.ofString "é")

[<Fact>]
let ``an emoji is two cells`` () =
    Assert.Equal(2, Width.ofString "\U0001F600")

[<Fact>]
let ``a zero width joiner is no cells`` () =
    Assert.Equal(0, Width.ofRune (System.Text.Rune 0x200D))

[<Fact>]
let ``a lone surrogate is one cell`` () =
    Assert.Equal(1, Width.ofString "\uD800")

[<Fact>]
let ``a control character is no cells`` () =
    Assert.Equal(2, Width.ofString "a\x1bb")

[<Fact>]
let ``the wide ranges are sorted and do not overlap`` () =
    Assert.NotEmpty(Width.wideRanges)

    for first, last in Width.wideRanges do
        Assert.True(first <= last, $"{first:X} > {last:X}")

    for (_, last), (next, _) in Array.pairwise Width.wideRanges do
        Assert.True(last < next, $"{last:X} overlaps {next:X}")

[<Fact>]
let ``a mark stays with the character before it`` () =
    let glyphs = Width.glyphs "éx"
    Assert.Equal<Glyph list>([ { Chars = "é"; Cells = 1 }; { Chars = "x"; Cells = 1 } ], glyphs)

[<Fact>]
let ``control characters and a mark with nothing before it are dropped`` () =
    let glyphs = Width.glyphs "́a\tb\r"
    Assert.Equal<Glyph list>([ { Chars = "a"; Cells = 1 }; { Chars = "b"; Cells = 1 } ], glyphs)

[<Fact>]
let ``a soft hyphen is one cell`` () = Assert.Equal(5, Width.ofString "co­op")

[<Fact>]
let ``bidi controls are dropped`` () =
    Assert.Equal<Glyph list>([ { Chars = "a"; Cells = 1 }; { Chars = "b"; Cells = 1 } ], Width.glyphs "a‮b⁦")
