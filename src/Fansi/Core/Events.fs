namespace Fansi.Core

/// A key as the terminal reports it, once the escape sequences are decoded.
type Key =
    | Char of char
    | Enter
    | Tab
    | Backspace
    | Delete
    | Esc
    | Up
    | Down
    | Left
    | Right
    | Home
    | End
    | PageUp
    | PageDown
    | Insert
    | F of int

type KeyEvent =
    { Key: Key
      Ctrl: bool
      Alt: bool
      Shift: bool }

    static member plain key =
        { Key = key
          Ctrl = false
          Alt = false
          Shift = false }

[<RequireQualifiedAccess>]
type MouseButton =
    | Left
    | Middle
    | Right
    | ScrollUp
    | ScrollDown
    | None

[<RequireQualifiedAccess>]
type MouseAction =
    | Press
    | Release
    | Move

type MouseEvent =
    { Button: MouseButton
      Action: MouseAction
      X: int
      Y: int
      Ctrl: bool
      Alt: bool
      Shift: bool }

/// Qualified because its Key case would otherwise shadow the Key type above.
[<RequireQualifiedAccess>]
type InputEvent =
    | Key of KeyEvent
    | Mouse of MouseEvent
    | Paste of string
    | Resize of width: int * height: int
    | FocusChanged of bool
