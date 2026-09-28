namespace Fansi

open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module SpinnerComponent =

    type SpinnerStyle =
        | Dots
        | Line
        | Braille
        | Custom of string array

    type Message = Tick of int64

    type Model =
        { Id: int64
          Frame: int
          Frames: string array
          Interval: int<ms>
          Style: Style
          Label: string }

    let private framesFor style =
        match style with
        | Dots -> [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
        | Line -> [| "|"; "/"; "-"; "\\" |]
        | Braille -> [| "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" |]
        | Custom frames -> frames

    let init spinnerStyle (interval: int<ms>) label =
        { Id = ComponentId.next ()
          Frame = 0
          Frames = framesFor spinnerStyle
          Interval = interval
          Style =
            { Style.Default with
                FgColor = Color.Cyan }
          Label = label },
        Cmd.none

    let update msg model =
        match msg with
        | Tick id when id = model.Id ->
            { model with
                Frame = (model.Frame + 1) % model.Frames.Length },
            Cmd.none
        | Tick _ -> model, Cmd.none

    let subscribe model =
        Sub.timer [ "fansi"; "spinner"; string model.Id ] model.Interval (Tick model.Id)

    let view model : Node =
        let frame = model.Frames[model.Frame]

        Ui.row
            [ Ui.text frame |> Ui.style model.Style
              if model.Label.Length > 0 then
                  Ui.text $" {model.Label}" ]
