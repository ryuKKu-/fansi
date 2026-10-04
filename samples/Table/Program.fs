module Fansi.Samples.Table

open Elmish
open Fansi
open Fansi.Core

type Proc =
    { Name: string
      Cpu: int
      Memory: int
      Status: string }

let private names =
    [ "init"
      "kworker"
      "sshd"
      "bash"
      "dotnet"
      "fansi-demo"
      "postgres"
      "nginx"
      "redis"
      "cron"
      "systemd-journal"
      "dbus"
      "日本語-agent"
      "chrome"
      "code"
      "node"
      "python"
      "rustc"
      "git"
      "vim"
      "tmux"
      "htop"
      "docker"
      "containerd"
      "kubelet"
      "📦-cache"
      "mysqld"
      "sshd-session"
      "pulseaudio"
      "Xorg" ]

let processes =
    names
    |> List.mapi (fun i name ->
        { Name = name
          Cpu = i * 37 % 100
          Memory = 50 + i * 113 % 900
          Status = if i % 5 = 0 then "sleeping" else "running" })

let private bind keys key description =
    Keymap.setHelp
        (Keymap.KeyBind.create keys)
        (Some
            { Keymap.Key = key
              Keymap.Description = description })

let keys =
    {| Up = bind [ KeyEvent.plain Key.Up ] "↑" "up"
       Down = bind [ KeyEvent.plain Key.Down ] "↓" "down"
       Page = bind [ KeyEvent.plain Key.PageUp; KeyEvent.plain Key.PageDown ] "pgup/pgdn" "page"
       Ends = bind [ KeyEvent.plain Key.Home; KeyEvent.plain Key.End ] "home/end" "first/last"
       Help = bind [ KeyEvent.plain (Key.Char '?') ] "?" "more"
       Quit = bind [ KeyEvent.plain Key.Esc ] "esc" "quit" |}

let private shortHelp = [ keys.Up; keys.Down; keys.Page; keys.Help; keys.Quit ]

let private fullHelp =
    [ [ keys.Up; keys.Down; keys.Page; keys.Ends ]; [ keys.Help; keys.Quit ] ]

let private columns: TableComponent.Column list =
    [ { TableComponent.Column.Title = "Name"
        TableComponent.Column.Width = Fill 1 }
      { TableComponent.Column.Title = "CPU"
        TableComponent.Column.Width = Len 5 }
      { TableComponent.Column.Title = "Memory"
        TableComponent.Column.Width = Len 8 }
      { TableComponent.Column.Title = "Status"
        TableComponent.Column.Width = Pct 15 } ]

let private cells (p: Proc) =
    [ Ui.text p.Name
      Ui.text (string p.Cpu + "%")
      Ui.text (string p.Memory + " MB")
      Ui.text p.Status
      |> Ui.fg (
          if p.Status = "running" then
              Color.Green
          else
              Color.BrightBlack
      ) ]

type Model =
    { Table: TableComponent.Model<Proc>
      Help: HelpComponent.Model }

// The title, the border's three lines, the header, the status line and up to
// four rows of full help.
let private chrome = 10

let init () =
    let table, _ = TableComponent.init columns cells 80 (24 - chrome) processes

    { Table = table
      Help =
        { HelpComponent.init () with
            Width = 80 } },
    Cmd.none

let update (msg: FansiMsg<unit>) (model: Model) =
    match msg with
    | KeyPress k when Keymap.``match`` keys.Quit k -> model, Cmd.quit
    | KeyPress k when Keymap.``match`` keys.Help k ->
        { model with
            Help = HelpComponent.toggle model.Help },
        Cmd.none
    | KeyPress k ->
        let table, _ = TableComponent.update (TableComponent.KeyInput k) model.Table
        { model with Table = table }, Cmd.none
    | Mouse m ->
        let table, _ = TableComponent.update (TableComponent.MouseInput m) model.Table
        { model with Table = table }, Cmd.none
    | Resize(w, h) ->
        { Table = TableComponent.setSize w (max 0 (h - chrome)) model.Table
          Help = { model.Help with Width = w } },
        Cmd.none
    | Paste _
    | FocusChanged _
    | App _ -> model, Cmd.none

let view model =
    let selected =
        TableComponent.selectedRow model.Table
        |> Option.map _.Name
        |> Option.defaultValue "nothing"

    Ui.col
        [ Ui.text "Processes" |> Ui.bold |> Ui.fg Color.Cyan |> Ui.len 1
          TableComponent.view model.Table |> Ui.fill 1
          Ui.text $"selected: {selected}" |> Ui.len 1
          HelpComponent.view shortHelp fullHelp model.Help ]

[<EntryPoint>]
let main _ =
    FansiProgram.mkProgram init update view |> FansiProgram.run
    0
