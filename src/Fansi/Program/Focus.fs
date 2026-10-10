namespace Fansi

/// Tracks which item in a fixed set has focus. One ring holds this state, so
/// two components cannot both have focus.
type Focus<'id when 'id: equality> = private { Items: 'id list; Index: int }

[<RequireQualifiedAccess>]
module Focus =

    let ofList (items: 'id list) : Focus<'id> =
        if List.isEmpty items then
            invalidArg (nameof items) "a focus ring needs at least one item"

        { Items = items; Index = 0 }

    let current (f: Focus<'id>) = f.Items[f.Index]

    let next (f: Focus<'id>) =
        { f with
            Index = (f.Index + 1) % List.length f.Items }

    let prev (f: Focus<'id>) =
        let count = List.length f.Items

        { f with
            Index = (f.Index + count - 1) % count }

    /// Moves focus to `id`. If `id` is not in the ring, focus does not change.
    /// The ring holds all items that can have focus.
    let focusOn (id: 'id) (f: Focus<'id>) =
        match List.tryFindIndex ((=) id) f.Items with
        | Some i -> { f with Index = i }
        | None -> f

    let isFocused (id: 'id) (f: Focus<'id>) = current f = id
