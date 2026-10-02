module Fansi.Tests.PropsTests

open Xunit
open Fansi.Core

[<Fact>]
let ``default props size themselves from their content and draw no border`` () =
    let p = Props.Default
    Assert.Equal(Auto, p.Main)
    Assert.Equal(Auto, p.Cross)
    Assert.Equal(Column, p.Direction)
    Assert.Equal(Justify.Start, p.Justify)
    Assert.Equal(Align.Stretch, p.Align)
    Assert.Equal(Edges.Zero, p.Padding)
    Assert.Equal(Edges.Zero, p.Margin)
    Assert.Equal(NoBorder, p.Border)

[<Fact>]
let ``default props have no title`` () = Assert.Empty(Props.Default.Title)
