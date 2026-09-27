namespace Fansi

open Fansi.Core

module Keymap =
    type Help = { Key: string; Description: string }

    type KeyControl =
        { Key: Key
          Ctrl: bool
          Alt: bool
          Shift: bool }

        static member plain key =
            { Key = key
              Ctrl = false
              Alt = false
              Shift = false }

    type KeyBind =
        { Keys: KeyControl list
          Disabled: bool
          Help: Help option }

        member this.Enabled = not this.Disabled && not (List.isEmpty this.Keys)

        static member create keys =
            { Keys = keys
              Disabled = false
              Help = None }

    let unbind keyBind = { keyBind with Keys = []; Help = None }

    let toggleEnable keyBind =
        { keyBind with
            Disabled = not keyBind.Disabled }

    let setHelp keyBind help = { keyBind with Help = help }

    let setKeys keyBind keys = { keyBind with Keys = keys }

    let ``match`` (binding: KeyBind) (event: KeyEvent) =
        binding.Enabled
        && binding.Keys
           |> List.exists (fun kc ->
               kc.Key = event.Key
               && kc.Ctrl = event.Ctrl
               && kc.Alt = event.Alt
               && kc.Shift = event.Shift)
