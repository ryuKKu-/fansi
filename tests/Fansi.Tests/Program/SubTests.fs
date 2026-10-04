module Fansi.Tests.SubTests

open System
open System.Threading
open Xunit
open Elmish
open Fansi

[<Fact>]
let ``none holds no subscription`` () = Assert.Empty(Sub.none: Sub<int>)

[<Fact>]
let ``batch keeps every subscription and its id`` () =
    let subs =
        Sub.batch [ Sub.timer [ "a" ] 1000<ms> 1; Sub.none; Sub.timer [ "b" ] 1000<ms> 2 ]

    Assert.Equal<string list list>([ [ "a" ]; [ "b" ] ], subs |> List.map fst)

[<Fact>]
let ``map keeps the id and wraps what the subscription dispatches`` () =
    let sub = Sub.timer [ "tick"; "7" ] 10<ms> 5 |> Sub.map (fun n -> n * 2)
    Assert.Equal<string list list>([ [ "tick"; "7" ] ], sub |> List.map fst)

    let got = new ManualResetEventSlim(false)
    let mutable received = 0
    let _, start = List.head sub

    use _running =
        start (fun m ->
            received <- m
            got.Set())

    Assert.True(got.Wait 2000, "the timer never fired")
    Assert.Equal(10, received)

[<Fact>]
let ``a timer stops once disposed`` () =
    // A ref cell, because a closure cannot take the address of a mutable local.
    let count = ref 0
    use fired = new ManualResetEventSlim(false)
    let _, start = Sub.timer [ "t" ] 10<ms> () |> List.head

    let running =
        start (fun () ->
            Interlocked.Increment(&count.contents) |> ignore
            fired.Set())

    Assert.True(fired.Wait 2000, "the timer never fired")
    running.Dispose()
    // A tick that was already running when Dispose was called may still finish,
    // so wait for the count to settle before checking that it stays put.
    let read () = Volatile.Read(&count.contents)
    let deadline = DateTime.UtcNow.AddSeconds 2.0
    let mutable stable = 0
    let mutable last = read ()

    while stable < 3 do
        if DateTime.UtcNow > deadline then
            failwith "the timer kept ticking for 2 s after Dispose"

        Thread.Sleep 50
        let now = read ()
        stable <- if now = last then stable + 1 else 0
        last <- now

    let stoppedAt = last
    Thread.Sleep 200
    Assert.Equal(stoppedAt, Volatile.Read(&count.contents))

[<Fact>]
let ``component ids are never reused`` () =
    let a = ComponentId.next ()
    let b = ComponentId.next ()
    Assert.NotEqual(a, b)
