namespace Fansi

open System

module Keymap =
    type Help = { Key: string; Description: string }

    type KeyControl =
        { Key: ConsoleKey
          Modifier: ConsoleModifiers option }

    type KeyBind =
        { Keys: KeyControl[]
          Disabled: bool
          Help: Help option }

        member this.Enabled = this.Disabled |> not && this.Keys <> [||]

        static member create key =
            { Keys = [| key |]
              Disabled = false
              Help = None }

        static member create keys =
            { Keys = keys
              Disabled = false
              Help = None }

    let unbind keyBind =
        { keyBind with
            Keys = [||]
            Help = None }

    let toggleEnable keyBind =
        { keyBind with
            Disabled = not keyBind.Disabled }

    let setHelp keyBind help = { keyBind with Help = help }

    let setKeys keyBind keys = { keyBind with Keys = keys }

    let ``match`` (binding: KeyBind) (key: ConsoleKeyInfo) =
        binding
        |> fun b ->
            b.Enabled
            && b.Keys
               |> Array.exists (fun kc ->
                   kc.Key = key.Key
                   && (kc.Modifier |> Option.defaultValue ConsoleModifiers.None) = key.Modifiers)
