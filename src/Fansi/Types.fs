namespace Fansi

open System

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
      Modifiers: ConsoleModifiers }

type FansiMsg<'appMsg> =
    | KeyPress of ConsoleKeyInfo
    | MouseEvent of MouseEvent
    | Quit
    | App of 'appMsg
