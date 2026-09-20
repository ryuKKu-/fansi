module Fansi.Tests.RendererTests

open Xunit
open Fansi.Core

[<Fact>]
let ``a buffer round-trips through the renderer's diff`` () =
    // SetFrame must accept a Buffer and keep its contents for the next diff
    let r = Fansi.Renderer.Renderer(60<Fansi.FPS>)
    let buf = Buffer.create 3 1
    Buffer.writeText buf { X = 0; Y = 0; Width = 3; Height = 1 } 0 0 Style.Default "abc"
    r.SetFrame buf
    Assert.Equal<string list>([ "abc" ], Buffer.toLines buf)
