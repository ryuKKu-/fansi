module Fansi.Tests.RendererTests

open System.IO
open Xunit
open Fansi
open Fansi.Core

let private renderer () =
    let sink = new StringWriter()
    Fansi.Renderer.Renderer(60<Fansi.FPS>, sink), sink

let private frame w h text =
    let buf = Buffer.create w h
    Buffer.writeText buf { X = 0; Y = 0; Width = w; Height = h } 0 0 Style.Default text
    buf

[<Fact>]
let ``the first frame emits its cells`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    Assert.Equal("\x1b[1;1Habc\x1b[0m", sink.ToString())

[<Fact>]
let ``a second frame emits only the run that changed`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    // a fresh buffer each frame: the renderer keeps the previous one as its diff baseline
    r.SetFrame(frame 3 1 "aXc")
    r.Flush()
    Assert.Equal("\x1b[1;2HX\x1b[0m", sink.ToString())

[<Fact>]
let ``an unchanged frame emits nothing`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    Assert.Equal("", sink.ToString())

[<Fact>]
let ``a run carries the style of its cells`` () =
    let r, sink = renderer ()
    let buf = Buffer.create 2 1
    let clip = { X = 0; Y = 0; Width = 2; Height = 1 }

    Buffer.writeText
        buf
        clip
        0
        0
        { Style.Default with
            FgColor = Color.Red
            Bold = true }
        "ab"

    r.SetFrame buf
    r.Flush()
    Assert.Equal("\x1b[1;1H\x1b[0m\x1b[1m\x1b[31mab\x1b[0m", sink.ToString())

[<Fact>]
let ``a frame at a new size starts by erasing the old one`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    r.SetFrame(frame 4 2 "ab")
    r.Flush()
    Assert.StartsWith(AnsiSequence.eraseVisibleScreen, sink.ToString())

[<Fact>]
let ``the erase leaves scrollback alone`` () =
    Assert.DoesNotContain("\x1b[3J", AnsiSequence.eraseVisibleScreen)

[<Fact>]
let ``a frame at the same size does not erase`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    r.SetFrame(frame 3 1 "aXc")
    r.Flush()
    Assert.DoesNotContain("\x1b[2J", sink.ToString())

[<Fact>]
let ``a blank frame at a new size still erases`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "abc")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    r.SetFrame(Buffer.create 5 2)
    r.Flush()
    Assert.StartsWith(AnsiSequence.eraseVisibleScreen, sink.ToString())

[<Fact>]
let ``nothing paints after stop`` () =
    let r, sink = renderer ()
    r.Start()
    r.SetFrame(frame 3 1 "abc")
    r.Stop()
    let atStop = sink.ToString()

    r.SetFrame(frame 3 1 "xyz")
    r.Flush()

    Assert.Contains("abc", atStop)
    Assert.Equal(atStop, sink.ToString())

[<Fact>]
let ``a wide character is written once and the next one follows it`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 3 1 "字a")
    r.Flush()
    Assert.Equal("\x1b[1;1H字a\x1b[0m", sink.ToString())

[<Fact>]
let ``a change to the right half reprints the whole wide character`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 2 1 "字")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    let next = frame 2 1 "字"
    // Set directly: Buffer.set would blank the left half, which is not the case here.
    next.Cells[1] <-
        { next.Cells[1] with
            Style = { Style.Default with Bold = true } }

    r.SetFrame next
    r.Flush()
    Assert.Equal("\x1b[1;1H字\x1b[0m", sink.ToString())

[<Fact>]
let ``narrow text over a wide character replaces both halves`` () =
    let r, sink = renderer ()
    r.SetFrame(frame 2 1 "字")
    r.Flush()
    sink.GetStringBuilder().Clear() |> ignore

    r.SetFrame(frame 2 1 "ab")
    r.Flush()
    Assert.Equal("\x1b[1;1Hab\x1b[0m", sink.ToString())
