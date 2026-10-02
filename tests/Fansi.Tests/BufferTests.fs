module Fansi.Tests.BufferTests

open Xunit
open Fansi.Core

let private all w h = { X = 0; Y = 0; Width = w; Height = h }

[<Fact>]
let ``a new buffer is all spaces`` () =
    let b = Buffer.create 3 2
    Assert.Equal<string list>([ "   "; "   " ], Buffer.toLines b)

[<Fact>]
let ``text is written at the given position`` () =
    let b = Buffer.create 5 2
    Buffer.writeText b (all 5 2) 1 1 Style.Default "abc"
    Assert.Equal<string list>([ "     "; " abc " ], Buffer.toLines b)

[<Fact>]
let ``text is cut at the clip rect`` () =
    let b = Buffer.create 5 1
    Buffer.writeText b { X = 0; Y = 0; Width = 3; Height = 1 } 0 0 Style.Default "abcde"
    Assert.Equal<string list>([ "abc  " ], Buffer.toLines b)

[<Fact>]
let ``writing outside the buffer is dropped rather than throwing`` () =
    let b = Buffer.create 3 1
    Buffer.writeText b (all 3 1) -2 0 Style.Default "abcde"
    Buffer.writeText b (all 3 1) 0 9 Style.Default "xyz"
    Assert.Equal<string list>([ "cde" ], Buffer.toLines b)

[<Fact>]
let ``fillRect paints a background over an area only`` () =
    let b = Buffer.create 4 3
    Buffer.writeText b (all 4 3) 0 0 Style.Default "aaaa"

    let cyan =
        { Style.Default with
            BgColor = Color.Cyan }

    Buffer.fillRect b (all 4 3) { X = 1; Y = 0; Width = 2; Height = 2 } cyan
    Assert.Equal(Color.Default, (Buffer.get b 0 0).Style.BgColor)
    Assert.Equal(Color.Cyan, (Buffer.get b 1 0).Style.BgColor)
    Assert.Equal(Color.Cyan, (Buffer.get b 2 1).Style.BgColor)
    Assert.Equal(Color.Default, (Buffer.get b 3 0).Style.BgColor)

[<Fact>]
let ``text keeps the background already under it`` () =
    let b = Buffer.create 2 1

    let blue =
        { Style.Default with
            BgColor = Color.Blue }

    Buffer.fillRect b (all 2 1) (all 2 1) blue
    Buffer.writeText b (all 2 1) 0 0 Style.Default "ab"
    Assert.Equal(Color.Blue, (Buffer.get b 0 0).Style.BgColor)
    Assert.Equal("a", (Buffer.get b 0 0).Symbol)

[<Fact>]
let ``an explicit background overrides what is under it`` () =
    let b = Buffer.create 2 1

    Buffer.fillRect
        b
        (all 2 1)
        (all 2 1)
        { Style.Default with
            BgColor = Color.Blue }

    Buffer.writeText
        b
        (all 2 1)
        0
        0
        { Style.Default with
            BgColor = Color.Red }
        "ab"

    Assert.Equal(Color.Red, (Buffer.get b 0 0).Style.BgColor)

[<Fact>]
let ``style is kept per cell`` () =
    let b = Buffer.create 2 1
    Buffer.writeText b (all 2 1) 0 0 { Style.Default with Bold = true } "ab"
    Assert.True((Buffer.get b 0 0).Style.Bold)

[<Fact>]
let ``a zero sized buffer is usable and empty`` () =
    let b = Buffer.create 0 0
    Buffer.writeText b (all 0 0) 0 0 Style.Default "abc"
    Assert.Equal<string list>([], Buffer.toLines b)

[<Fact>]
let ``a wide character takes two cells`` () =
    let b = Buffer.create 3 1
    Buffer.writeText b (all 3 1) 0 0 Style.Default "字a"
    Assert.Equal<string list>([ "字a" ], Buffer.toLines b)
    Assert.Equal("字", (Buffer.get b 0 0).Symbol)
    Assert.True((Buffer.get b 1 0).Continuation)
    Assert.Equal("a", (Buffer.get b 2 0).Symbol)

[<Fact>]
let ``writing over the left half of a wide character blanks the right half`` () =
    let b = Buffer.create 2 1
    Buffer.writeText b (all 2 1) 0 0 Style.Default "字"
    Buffer.writeText b (all 2 1) 0 0 Style.Default "x"
    Assert.Equal<string list>([ "x " ], Buffer.toLines b)
    Assert.False((Buffer.get b 1 0).Continuation)

[<Fact>]
let ``writing over the right half of a wide character blanks the left half`` () =
    let b = Buffer.create 2 1
    Buffer.writeText b (all 2 1) 0 0 Style.Default "字"
    Buffer.writeText b (all 2 1) 1 0 Style.Default "x"
    Assert.Equal<string list>([ " x" ], Buffer.toLines b)

[<Fact>]
let ``a wide character cut by the clip rect becomes a space`` () =
    let b = Buffer.create 3 1
    Buffer.writeText b { X = 0; Y = 0; Width = 2; Height = 1 } 1 0 Style.Default "字"
    Assert.Equal<string list>([ "   " ], Buffer.toLines b)
    Assert.False((Buffer.get b 2 0).Continuation)

[<Fact>]
let ``a wide character at the buffer's right edge becomes a space`` () =
    let b = Buffer.create 2 1
    Buffer.writeText b (all 2 1) 1 0 Style.Default "字"
    Assert.Equal<string list>([ "  " ], Buffer.toLines b)

[<Fact>]
let ``control characters are never written`` () =
    let b = Buffer.create 6 1
    Buffer.writeText b (all 6 1) 0 0 Style.Default "a\x1b[2Jb"
    Assert.Equal<string list>([ "a[2Jb " ], Buffer.toLines b)

[<Fact>]
let ``a background fill keeps a wide character whole`` () =
    let b = Buffer.create 2 1
    Buffer.writeText b (all 2 1) 0 0 Style.Default "字"

    Buffer.fillRect
        b
        (all 2 1)
        (all 2 1)
        { Style.Default with
            BgColor = Color.Cyan }

    Assert.Equal<string list>([ "字" ], Buffer.toLines b)
    Assert.True((Buffer.get b 1 0).Continuation)
    Assert.Equal(Color.Cyan, (Buffer.get b 0 0).Style.BgColor)
    Assert.Equal(Color.Cyan, (Buffer.get b 1 0).Style.BgColor)

[<Fact>]
let ``a fill over one half of a wide character keeps both halves`` () =
    let b = Buffer.create 2 1
    Buffer.writeText b (all 2 1) 0 0 Style.Default "字"

    Buffer.fillRect
        b
        (all 2 1)
        { X = 1; Y = 0; Width = 1; Height = 1 }
        { Style.Default with
            BgColor = Color.Cyan }

    Assert.Equal<string list>([ "字" ], Buffer.toLines b)
    Assert.Equal("字", (Buffer.get b 0 0).Symbol)
    Assert.True((Buffer.get b 1 0).Continuation)
