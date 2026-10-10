module Fansi.Tests.SolverTests

open Xunit
open FsCheck.Xunit
open Fansi.Core

[<Fact>]
let ``equal weights split evenly when the total divides`` () =
    Assert.Equal<int list>([ 5; 5 ], Solver.distribute 10 [ 1; 1 ])

[<Fact>]
let ``a remainder goes to the earlier entry`` () =
    // 3 split two ways is 1 each with 1 spare. The lower index wins the tie.
    Assert.Equal<int list>([ 2; 1 ], Solver.distribute 3 [ 1; 1 ])

[<Fact>]
let ``weights scale the shares`` () =
    Assert.Equal<int list>([ 10; 20; 30 ], Solver.distribute 60 [ 1; 2; 3 ])

[<Fact>]
let ``zero weights never receive a share`` () =
    Assert.Equal<int list>([ 0; 7; 0 ], Solver.distribute 7 [ 0; 1; 0 ])

[<Fact>]
let ``no weight at all gives nothing away`` () =
    Assert.Equal<int list>([ 0; 0 ], Solver.distribute 10 [ 0; 0 ])

[<Fact>]
let ``a non-positive total gives nothing away`` () =
    Assert.Equal<int list>([ 0; 0 ], Solver.distribute 0 [ 1; 1 ])
    Assert.Equal<int list>([ 0; 0 ], Solver.distribute -5 [ 1; 1 ])

[<Fact>]
let ``negative weights are clamped to zero and do not receive shares`` () =
    Assert.Equal<int list>([ 0; 10 ], Solver.distribute 10 [ -1; 3 ])

[<Fact>]
let ``negative weights do not prevent positive clamped weights from receiving shares`` () =
    Assert.Equal<int list>([ 0; 10 ], Solver.distribute 10 [ -5; 1 ])

[<Fact>]
let ``the surplus tie-break still favours the lowest index at three equal weights`` () =
    // Pinned so the int64 overflow fix cannot change this behaviour.
    Assert.Equal<int list>([ 34; 33; 33 ], Solver.distribute 100 [ 1; 1; 1 ])

[<Fact>]
let ``a large total does not overflow when multiplied by a small weight`` () =
    // total * weight (2_000_000_000 * 2) overflows int32 well before either
    // operand looks dangerous on its own.
    let sizes = Solver.solve [ Fill 2 ] [ 0 ] 2_000_000_000
    Assert.True(List.forall (fun s -> s >= 0) sizes)
    Assert.Equal(2_000_000_000, List.sum sizes)

[<Fact>]
let ``a large total splits across mixed weights without overflowing`` () =
    let shares = Solver.distribute 2_000_000_000 [ 1; 3 ]
    Assert.True(List.forall (fun s -> s >= 0) shares)
    Assert.Equal(2_000_000_000, List.sum shares)

[<Fact>]
let ``distribute does not overflow when weights are near the top of the int range`` () =
    // Two weights of Int32.MaxValue push totalWeight itself past Int32.MaxValue.
    // This used to throw OverflowException in the totalWeight sum.
    Assert.Equal<int list>([ 50; 50 ], Solver.distribute 100 [ System.Int32.MaxValue; System.Int32.MaxValue ])

[<Fact>]
let ``distribute keeps the tie-break correct with huge weights and a tiny total`` () =
    // Both raw shares floor to 0 here. The tie-break must still pick the earlier
    // index. An early conversion to `int` must not corrupt the remainder
    // (2_147_483_647, right at the int32 boundary).
    Assert.Equal<int list>([ 1; 0 ], Solver.distribute 1 [ System.Int32.MaxValue; System.Int32.MaxValue ])

[<Fact>]
let ``asymmetric weights do not invert the tie-break when the remainder wraps as an int`` () =
    // totalWeight here is 2_987_867_508, past Int32.MaxValue. The true int64
    // remainders are [2_656_725_540; 331_141_968]. Narrowed to `int`, the first
    // one wraps to a negative number and sorts *below* the second. This flips the
    // tie-break winner. The test pins the remainder as `int64` end to end.
    // Narrowed, the result would be [232; 166] instead.
    Assert.Equal<int list>([ 233; 165 ], Solver.distribute 398 [ 1748346702; 1239520806 ])

[<Fact>]
let ``a fill weight near the top of the int range does not overflow the container`` () =
    let sizes =
        Solver.solve [ Fill System.Int32.MaxValue; Fill System.Int32.MaxValue ] [ 0; 0 ] 100

    Assert.True(List.forall (fun s -> s >= 0) sizes)
    Assert.Equal(100, List.sum sizes)

[<Fact>]
let ``two huge lengths overflowing a small container do not overflow the sum`` () =
    let sizes =
        Solver.solve [ Len System.Int32.MaxValue; Len System.Int32.MaxValue ] [ 0; 0 ] 100

    Assert.True(List.forall (fun s -> s >= 0) sizes)
    Assert.Equal(100, List.sum sizes)

[<Property>]
let ``shares sum to the total whenever there is a positive weight`` (total: int) (weights: int list) =
    let weights = weights |> List.map (fun w -> w % 10)
    let total = abs total % 500
    let shares = Solver.distribute total weights
    let clampedWeights = weights |> List.map (max 0)

    let expected =
        if total > 0 && List.sum clampedWeights > 0 then
            total
        else
            0

    List.sum shares = expected

[<Property>]
let ``shares are never negative and never longer than the weights`` (total: int) (weights: int list) =
    let weights = weights |> List.map (fun w -> w % 10)
    let shares = Solver.distribute (abs total % 500) weights
    List.length shares = List.length weights && List.forall (fun s -> s >= 0) shares

let private solve cs intrinsics available = Solver.solve cs intrinsics available

[<Fact>]
let ``a fixed length is taken literally`` () =
    Assert.Equal<int list>([ 30 ], solve [ Len 30 ] [ 0 ] 80)

[<Fact>]
let ``auto takes the intrinsic size`` () =
    Assert.Equal<int list>([ 12 ], solve [ Auto ] [ 12 ] 80)

[<Fact>]
let ``a percentage is taken of the available space`` () =
    Assert.Equal<int list>([ 20 ], solve [ Pct 25 ] [ 0 ] 80)

[<Fact>]
let ``a ratio is taken of the available space`` () =
    Assert.Equal<int list>([ 27 ], solve [ Ratio(1, 3) ] [ 0 ] 81)

[<Fact>]
let ``max caps the intrinsic size`` () =
    Assert.Equal<int list>([ 10 ], solve [ Max 10 ] [ 40 ] 80)
    Assert.Equal<int list>([ 4 ], solve [ Max 10 ] [ 4 ] 80)

[<Fact>]
let ``fill shares what is left after the fixed children`` () =
    // 30 fixed, 50 left, split 1:1
    Assert.Equal<int list>([ 30; 25; 25 ], solve [ Len 30; Fill 1; Fill 1 ] [ 0; 0; 0 ] 80)

[<Fact>]
let ``fill weights are proportional`` () =
    Assert.Equal<int list>([ 20; 40 ], solve [ Fill 1; Fill 2 ] [ 0; 0 ] 60)

[<Fact>]
let ``fill children absorb the rounding rather than losing cells`` () =
    // 100 split three ways cannot be exact; the total must still be 100
    let sizes = solve [ Fill 1; Fill 1; Fill 1 ] [ 0; 0; 0 ] 100
    Assert.Equal(100, List.sum sizes)
    Assert.Equal<int list>([ 34; 33; 33 ], sizes)

[<Fact>]
let ``min takes at least its floor and grows with what is left`` () =
    Assert.Equal<int list>([ 10; 70 ], solve [ Len 10; Min 5 ] [ 0; 0 ] 80)

[<Fact>]
let ``overflow shrinks the last child first`` () =
    // 60 + 60 wanted, 80 available, so 40 must come off the tail
    Assert.Equal<int list>([ 60; 20 ], solve [ Len 60; Len 60 ] [ 0; 0 ] 80)

[<Fact>]
let ``overflow keeps shrinking backwards when the tail is not enough`` () =
    Assert.Equal<int list>([ 40; 0; 0 ], solve [ Len 50; Len 30; Len 30 ] [ 0; 0; 0 ] 40)

[<Fact>]
let ``no children gives no sizes`` () =
    Assert.Equal<int list>([], solve [] [] 80)

[<Fact>]
let ``a negative available size gives zeroes`` () =
    Assert.Equal<int list>([ 0; 0 ], solve [ Fill 1; Fill 1 ] [ 0; 0 ] -10)

[<Fact>]
let ``a negative length clamps to zero`` () =
    Assert.Equal<int list>([ 0 ], solve [ Len -5 ] [ 0 ] 80)

[<Fact>]
let ``a negative fill weight is not a grower and takes no space`` () =
    Assert.Equal<int list>([ 0 ], solve [ Fill -1 ] [ 0 ] 80)

[<Fact>]
let ``a negative min floors at zero but still grows`` () =
    Assert.Equal<int list>([ 80 ], solve [ Min -3 ] [ 0 ] 80)

[<Fact>]
let ``a negative percentage clamps to zero`` () =
    Assert.Equal<int list>([ 0 ], solve [ Pct -10 ] [ 0 ] 80)

[<Fact>]
let ``a percentage does not overflow at the top of the int range`` () =
    Assert.Equal<int list>([ System.Int32.MaxValue ], solve [ Pct 100 ] [ 0 ] System.Int32.MaxValue)

[<Fact>]
let ``a ratio does not overflow at the top of the int range`` () =
    Assert.Equal<int list>([ System.Int32.MaxValue ], solve [ Ratio(1, 1) ] [ 0 ] System.Int32.MaxValue)

[<Fact>]
let ``summing huge lengths during overflow does not overflow either`` () =
    let sizes =
        solve [ Len System.Int32.MaxValue; Len System.Int32.MaxValue ] [ 0; 0 ] 10

    Assert.Equal(10, List.sum sizes)
    Assert.Equal<int list>([ 10; 0 ], sizes)

[<Fact>]
let ``a mismatched intrinsics list raises naming both counts`` () =
    let ex =
        Assert.Throws<System.ArgumentException>(fun () -> Solver.solve [ Len 1; Len 2 ] [ 0 ] 80 |> ignore)

    Assert.Contains("2 constraints", ex.Message)
    Assert.Contains("1 intrinsics", ex.Message)

// Field values are signed (no abs), so negative constraints are reachable.
// Only the variant choice uses abs, as an index. `available` in the properties
// below is the raw generated int. It can land anywhere in the int32 range,
// including near Int32.MaxValue, where Pct/Ratio used to overflow.
let private constraintGen (n: int) =
    match abs n % 7 with
    | 0 -> Auto
    | 1 -> Len(n % 30)
    | 2 -> Pct(n % 120)
    | 3 -> Ratio(n % 5, n % 4)
    | 4 -> Fill(n % 4)
    | 5 -> Min(n % 20)
    | _ -> Max(n % 20)

let private isGrower c =
    match c with
    | Fill w -> w > 0
    | Min _ -> true
    | _ -> false

[<Property>]
let ``sizes are never negative`` (seeds: int list) (available: int) =
    let cs = seeds |> List.map constraintGen
    let intrinsics = seeds |> List.map (fun n -> abs n % 20)
    let sizes = Solver.solve cs intrinsics available
    List.forall (fun s -> s >= 0) sizes

[<Property>]
let ``sizes never exceed the available space`` (seeds: int list) (available: int) =
    let cs = seeds |> List.map constraintGen
    let intrinsics = seeds |> List.map (fun n -> abs n % 20)
    List.sum (Solver.solve cs intrinsics available) <= max 0 available

[<Property>]
let ``a grower makes the children fill the space exactly`` (seeds: int list) (available: int) =
    let cs = seeds |> List.map constraintGen
    let intrinsics = seeds |> List.map (fun n -> abs n % 20)
    let sizes = Solver.solve cs intrinsics available
    not (List.exists isGrower cs) || List.sum sizes = max 0 available

[<Property>]
let ``one size comes back per constraint`` (seeds: int list) (available: int) =
    let cs = seeds |> List.map constraintGen
    let intrinsics = seeds |> List.map (fun n -> abs n % 20)
    List.length (Solver.solve cs intrinsics available) = List.length cs
