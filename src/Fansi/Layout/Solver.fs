namespace Fansi.Core

module Solver =

    /// Split `total` into parts proportional to `weights`, summing to exactly `total`.
    /// Integer division leaves a surplus; it goes to the entries with the largest
    /// fractional part, with ties won by the lower index. Without this the parts sum
    /// to less than the total and the missing cells show up as gaps on screen.
    let distribute (total: int) (weights: int list) : int list =
        // A weight can be as large as Int32.MaxValue (Ui.fill takes an unconstrained
        // int), and F#'s List.sumBy uses checked addition even for int, so two such
        // weights would throw OverflowException here before any other guard runs.
        let totalWeight = weights |> List.sumBy (fun w -> int64 (max 0 w))

        if total <= 0 || totalWeight <= 0L then
            weights |> List.map (fun _ -> 0)
        else
            // total * weight can overflow int32 well before either operand is
            // individually large (e.g. a fill weight of 2 at a 2-billion-cell
            // total), so the share multiplication runs in int64.
            let total64 = int64 total

            let shares =
                weights |> List.map (fun w -> int (total64 * int64 (max 0 w) / totalWeight))

            let surplus = total - List.sum shares

            // The remainder is only ever compared and sorted, never returned, so it
            // stays int64: totalWeight can now exceed Int32.MaxValue itself (several
            // large weights), and converting the remainder down to `int` would wrap.
            let winners =
                weights
                |> List.mapi (fun i w -> i, w, total64 * int64 (max 0 w) % totalWeight)
                |> List.filter (fun (_, w, _) -> w > 0)
                |> List.sortByDescending (fun (i, _, remainder) -> remainder, -i)
                |> List.truncate surplus
                |> List.map (fun (i, _, _) -> i)
                |> Set.ofList

            shares |> List.mapi (fun i s -> if winners.Contains i then s + 1 else s)

    let private isGrower c =
        match c with
        | Fill w -> w > 0
        | Min _ -> true
        | _ -> false

    let private growWeight c =
        match c with
        | Fill w -> max 0 w
        | Min _ -> 1
        | _ -> 0

    let private baseSize (available: int) (c: Constraint) (intrinsic: int) =
        let intrinsic = max 0 intrinsic

        match c with
        | Len n -> max 0 n
        // available * p can overflow int32 (e.g. Pct 100 at Int32.MaxValue), so
        // the multiplication runs in int64 and the result is clamped back down.
        | Pct p -> int (min (int64 available) (int64 available * int64 (max 0 p) / 100L))
        | Ratio(a, b) ->
            if b <= 0 then
                0
            else
                int (min (int64 available) (int64 available * int64 (max 0 a) / int64 b))
        | Auto -> intrinsic
        | Max n -> min (max 0 n) intrinsic
        | Min n -> max 0 n
        | Fill _ -> 0

    /// Size each child along one axis. `intrinsics` holds each child's measured
    /// content size, in the same order as `constraints`. The two lists must have
    /// the same length; a mismatch raises `ArgumentException` naming both counts,
    /// since that means the caller built its lists wrong, not that the geometry
    /// is merely awkward.
    ///
    /// Sizes are never negative and never sum past `available`. When any child can
    /// grow, they sum to `available` exactly, so the children tile their parent with
    /// no gap and no overlap.
    let solve (constraints: Constraint list) (intrinsics: int list) (available: int) : int list =
        if List.length constraints <> List.length intrinsics then
            invalidArg
                "intrinsics"
                $"solve expects one intrinsic per constraint, got %d{List.length constraints} constraints and %d{List.length intrinsics} intrinsics"

        let available = max 0 available
        let bases = List.map2 (baseSize available) constraints intrinsics
        // Len is not clamped to `available`, so a handful of large children can
        // overflow an int32 sum; add them up as int64 instead.
        let wanted = bases |> List.sumBy int64

        if wanted <= int64 available then
            if List.exists isGrower constraints then
                let extra = distribute (available - int wanted) (List.map growWeight constraints)
                List.map2 (+) bases extra
            else
                bases
        else
            // Shrink from the last child backwards. Nothing has a floor here: a child
            // that cannot fit is better collapsed than left overflowing its parent.
            // The excess is threaded as int64 for the same overflow reason as `wanted`.
            let shrunk, _ =
                bases
                |> List.rev
                |> List.mapFold
                    (fun (excess: int64) b ->
                        let taken = min excess (int64 b)
                        b - int taken, excess - taken)
                    (wanted - int64 available)

            List.rev shrunk
