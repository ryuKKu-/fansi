namespace Fansi

open System
open Elmish

[<RequireQualifiedAccess>]
module TimerComponent =
    
    type Message =
        | TickMsg of int
        | StartStopMsg of int
        | TimedOutMsg of int
    
    type Model = {
        Id: int
        Running: bool
        Interval: float
        Timeout: TimeSpan
    }
    with
        member this.Toggle() = Cmd.ofMsg (StartStopMsg this.Id)
    
    let init id interval timeout =
        { Id = id; Interval = interval; Running = true; Timeout = timeout }, Cmd.none
    
    let update msg model =
        match msg with
        | TickMsg id ->
            if id = model.Id then
                let t = model.Timeout - TimeSpan.FromMilliseconds(model.Interval)
                if t <= TimeSpan.Zero then
                    { model with Timeout = TimeSpan.Zero; Running = false }, Cmd.ofMsg (TimedOutMsg id)
                else
                    { model with Timeout = t }, Cmd.none
            else
                model, Cmd.none

        | StartStopMsg id ->
            if id = model.Id && model.Timeout > TimeSpan.Zero then
                { model with Running = model.Running |> not }, Cmd.none
            else
                model, Cmd.none

        | _ ->
            model, Cmd.none
                
    let view model =
        model.Timeout.ToString()
        
    let subscribe model =
        [ if model.Running then
            ["timer"; string model.Id], Sub.timer model.Interval (Message.TickMsg model.Id) ]

