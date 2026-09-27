module Fansi.Tests.FocusTests

open Xunit
open Fansi

type private Panel =
    | Left
    | Middle
    | Right

[<Fact>]
let ``focus starts on the first item`` () =
    let f = Focus.ofList [ Left; Middle; Right ]
    Assert.Equal(Left, Focus.current f)

[<Fact>]
let ``next walks forward and wraps`` () =
    let f = Focus.ofList [ Left; Middle; Right ]
    Assert.Equal(Middle, Focus.current (Focus.next f))
    Assert.Equal(Right, Focus.current (f |> Focus.next |> Focus.next))
    Assert.Equal(Left, Focus.current (f |> Focus.next |> Focus.next |> Focus.next))

[<Fact>]
let ``prev walks backward and wraps`` () =
    let f = Focus.ofList [ Left; Middle; Right ]
    Assert.Equal(Right, Focus.current (Focus.prev f))
    Assert.Equal(Middle, Focus.current (f |> Focus.prev |> Focus.prev))

[<Fact>]
let ``focusOn moves to a known id and ignores an unknown one`` () =
    let f = Focus.ofList [ Left; Middle; Right ]
    Assert.Equal(Right, Focus.current (Focus.focusOn Right f))
    Assert.Equal(Left, Focus.current (Focus.focusOn Left (Focus.focusOn Right f)))

    // Starts off the first item, so a miss that reset to index 0 would show
    let partial = Focus.ofList [ Left; Middle ] |> Focus.next
    Assert.Equal(Middle, Focus.current (Focus.focusOn Right partial))

[<Fact>]
let ``isFocused answers for exactly one item`` () =
    let f = Focus.ofList [ Left; Middle; Right ] |> Focus.next
    Assert.False(Focus.isFocused Left f)
    Assert.True(Focus.isFocused Middle f)
    Assert.False(Focus.isFocused Right f)

[<Fact>]
let ``a single item ring stays on that item`` () =
    let f = Focus.ofList [ Left ]
    Assert.Equal(Left, Focus.current (Focus.next f))
    Assert.Equal(Left, Focus.current (Focus.prev f))

[<Fact>]
let ``an empty ring is rejected rather than silently allowed`` () =
    Assert.Throws<System.ArgumentException>(fun () -> Focus.ofList ([]: Panel list) |> ignore)
    |> ignore
