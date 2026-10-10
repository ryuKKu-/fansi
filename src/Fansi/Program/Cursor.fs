namespace Fansi

open Elmish
open Fansi
open Fansi.Core

/// A text cursor for any component that edits text. Focus is not part of the
/// model. The caller passes it in, so the focus ring stays the only place that
/// knows which component has focus.
[<RequireQualifiedAccess>]
module Cursor =

    type CursorId = int64

    type CursorType =
        | Blink
        | Static
        | Hidden

    type Message = BlinkTick of CursorId

    type Model =
        { Id: CursorId
          Type: CursorType
          BlinkSpeed: int<ms>
          Blink: bool }

    let create () =
        { Id = ComponentId.next ()
          Type = Blink
          BlinkSpeed = 530<ms>
          Blink = true }

    let update msg model =
        match msg with
        | BlinkTick id when id = model.Id && model.Type = Blink -> { model with Blink = not model.Blink }, Cmd.none
        | BlinkTick _ -> model, Cmd.none

    let view (focused: bool) (under: string) (style: Style) (model: Model) : Node =
        let shown =
            focused
            && match model.Type with
               | Blink -> model.Blink
               | Static -> true
               | Hidden -> false

        let style =
            if shown then
                { style with
                    FgColor = Color.Black
                    BgColor = Color.Cyan }
            else
                style

        Ui.text under |> Ui.style style

    let subscribe (focused: bool) (model: Model) =
        if focused && model.Type = Blink then
            Sub.timer [ "fansi"; "cursor"; string model.Id ] model.BlinkSpeed (BlinkTick model.Id)
        else
            Sub.none
