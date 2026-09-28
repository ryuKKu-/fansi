namespace Fansi

open System
open Elmish
open Fansi
open Fansi.Core

[<RequireQualifiedAccess>]
module TimerComponent =

    type Message =
        | TickMsg of int64
        | StartStopMsg of int64
        | TimedOutMsg of int64

    type Model =
        { Id: int64
          Running: bool
          Interval: int<ms>
          Timeout: TimeSpan
          Style: Style }

        member this.Toggle() = Cmd.ofMsg (StartStopMsg this.Id)

    let init (interval: int<ms>) timeout =
        { Id = ComponentId.next ()
          Interval = interval
          Running = true
          Timeout = timeout
          Style = Style.Default },
        Cmd.none

    let update msg model =
        match msg with
        | TickMsg id when id = model.Id && model.Running ->
            let t = model.Timeout - TimeSpan.FromMilliseconds(float (int model.Interval))

            if t <= TimeSpan.Zero then
                { model with
                    Timeout = TimeSpan.Zero
                    Running = false },
                Cmd.ofMsg (TimedOutMsg id)
            else
                { model with Timeout = t }, Cmd.none

        | StartStopMsg id when id = model.Id && model.Timeout > TimeSpan.Zero ->
            { model with
                Running = not model.Running },
            Cmd.none

        | TickMsg _
        | StartStopMsg _
        | TimedOutMsg _ -> model, Cmd.none

    let view model : Node =
        let fmt = @"hh\:mm\:ss\.fff"
        let timeStr = model.Timeout.ToString(fmt)
        let label = if model.Running then "⏱ " + timeStr else "⏸ " + timeStr
        Ui.text label |> Ui.style model.Style

    let subscribe model =
        if model.Running then
            Sub.timer [ "fansi"; "timer"; string model.Id ] model.Interval (TickMsg model.Id)
        else
            Sub.none
