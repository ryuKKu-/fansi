namespace Fansi

open Fansi.Core

/// Milliseconds, for Cmd.after and the input timeouts.
[<Measure>]
type ms

/// Mirrors InputEvent, with App added for the application's own messages. The
/// program converts one to the other and dispatches.
type FansiMsg<'appMsg> =
    | KeyPress of KeyEvent
    | Mouse of MouseEvent
    | Paste of string
    | Resize of width: int * height: int
    | FocusChanged of bool
    | App of 'appMsg

/// Ids for components that own a subscription. Allocated once per instance, so two
/// instances of the same component never share a subscription key.
module internal ComponentId =
    let mutable private last = 0L

    let next () =
        System.Threading.Interlocked.Increment(&last)
