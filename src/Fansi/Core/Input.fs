namespace Fansi

open System

module Input =
    /// Parse SGR mouse escape sequence: \x1b[<button;x;y;M or \x1b[<button;x;y;m
    let tryParseMouseEvent (buffer: string) : MouseEvent option =
        if buffer.Length < 6 then
            Option.None
        elif not (buffer.StartsWith("\x1b[<")) then
            Option.None
        else
            let payload = buffer.Substring(3)
            let termChar = payload[payload.Length - 1]
            let data = payload.Substring(0, payload.Length - 1)
            let parts = data.Split(';')

            if parts.Length <> 3 then
                Option.None
            else
                match Int32.TryParse(parts[0]), Int32.TryParse(parts[1]), Int32.TryParse(parts[2]) with
                | (true, buttonCode), (true, x), (true, y) ->
                    let action =
                        if termChar = 'm' then MouseAction.Release
                        elif buttonCode &&& 32 <> 0 then MouseAction.Move
                        else MouseAction.Press

                    let baseButton = buttonCode &&& 3
                    let isScroll = buttonCode &&& 64 <> 0

                    let button =
                        if isScroll then
                            if baseButton = 0 then
                                MouseButton.ScrollUp
                            else
                                MouseButton.ScrollDown
                        else
                            match baseButton with
                            | 0 -> MouseButton.Left
                            | 1 -> MouseButton.Middle
                            | 2 -> MouseButton.Right
                            | _ -> MouseButton.None

                    let mods =
                        let mutable m = ConsoleModifiers.None

                        if buttonCode &&& 4 <> 0 then
                            m <- m ||| ConsoleModifiers.Shift

                        if buttonCode &&& 8 <> 0 then
                            m <- m ||| ConsoleModifiers.Alt

                        if buttonCode &&& 16 <> 0 then
                            m <- m ||| ConsoleModifiers.Control

                        m

                    Some
                        { Button = button
                          Action = action
                          X = x - 1
                          Y = y - 1
                          Modifiers = mods }
                | _ -> Option.None

    /// Check if a point (x, y) is inside a rectangle
    let hitTest (x: int) (y: int) (rect: Core.Rect) =
        x >= rect.X
        && x < rect.X + rect.Width
        && y >= rect.Y
        && y < rect.Y + rect.Height
