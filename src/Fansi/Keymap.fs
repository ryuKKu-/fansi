namespace Fansi

open Fansi.Core

module Keymap =
    type Help = { Key: string; Description: string }

    type KeyBind =
        { Keys: KeyEvent list
          Disabled: bool
          Help: Help option }

        member this.Enabled = not this.Disabled && not (List.isEmpty this.Keys)

        static member create(keys: KeyEvent list) =
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
        binding.Enabled && List.contains event binding.Keys
