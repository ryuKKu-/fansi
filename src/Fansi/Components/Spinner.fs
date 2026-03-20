namespace Fansi

open Elmish
open Fansi.Core

[<RequireQualifiedAccess>]
module SpinnerComponent =

    type SpinnerStyle =
        | Dots
        | Line
        | Braille
        | Custom of string array

    type Message =
        | Tick

    type Model =
        { Frame: int
          Frames: string array
          Interval: float
          Style: Style
          Label: string }

    let private framesFor style =
        match style with
        | Dots -> [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
        | Line -> [| "|"; "/"; "-"; "\\" |]
        | Braille -> [| "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" |]
        | Custom frames -> frames

    let init spinnerStyle interval label =
        { Frame = 0
          Frames = framesFor spinnerStyle
          Interval = interval
          Style = { Style.Default with FgColor = Color.Cyan }
          Label = label },
        Cmd.none

    let update msg model =
        match msg with
        | Tick ->
            { model with Frame = (model.Frame + 1) % model.Frames.Length }, Cmd.none

    let subscribe model =
        [ [ "spinner" ], Sub.timer model.Interval Tick ]

    let view model : Node =
        let frame = model.Frames[model.Frame]
        Node.row [
            Node.styledText model.Style frame
            if model.Label.Length > 0 then
                Node.text $" {model.Label}"
        ]
