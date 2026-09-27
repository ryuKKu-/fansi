# Interactive sample

A small typing demo for the Fansi raw-mode input framework. It has two panels — a
Typing panel and an Events panel — and a status line showing the last key, the
last paste's length, and the terminal size. Tab and Shift+Tab switch focus
between the panels.

Raw mode, the reader thread, the escape timeout, and resize cannot be tested
automatically. This sample is how a person checks them by hand.

## Run it

```
dotnet run --project samples/Interactive
```

## Keys

- Type letters — they appear in the Typing panel.
- Backspace — removes the last character.
- Tab — moves focus to the next panel.
- Shift+Tab — moves focus to the previous panel.
- Esc — quits.
- Ctrl+C — quits.
- Ctrl+T — arms a quit two seconds later, from a timer, with no more keys needed.
- Ctrl+E — throws on purpose, to check the program fails loudly rather than
  freezing.

Any other key, a mouse click, a resize, or a terminal-focus change shows up as a
line in the Events panel.

## Acceptance checklist

Work down this list on a real terminal. None of it can be checked from an
environment with no console attached.

1. **Type some letters.** They should not echo twice and should not need Enter —
   they appear in the Typing panel's `> ...` line as you press them. Press
   Backspace and confirm the last character is removed.
   Then type `é`. It should appear as `é`, not be dropped and not turn into
   another character.
2. **Press Esc once.** The program should quit on that single press, within
   about 50 ms, not need a second press. A noticeable pause before it quits is
   worth reporting.
3. **Press Ctrl+T, then leave the keyboard alone for about two seconds.** The
   Events panel should log "quitting from a timer in 2s - hands off the
   keyboard", and the program should quit on its own about two seconds later
   with no further key pressed.
4. **Press Ctrl+C.** The program should exit normally, the same way Esc does,
   not be killed by the operating system. (The Events panel may log the key
   too, but it can flash past too quickly to read — the thing that matters is a
   normal exit.)
5. **Press an arrow key, then a function key (F1-F12).** Each should show as
   its own name (`Up`, `Down`, `F 1`, and so on) in the Events panel, one line
   per press — not as a stray Escape followed by letters or digits.
6. **Hold Alt and press F.** The logged line should show `alt=True`, not a
   separate Escape event followed by a plain `f`.
7. **Paste a paragraph of text (over 64 KiB is the case that spans several reads).** The
   status line's "last paste" and the Events panel should both show one
   `pasted N characters` line for the whole paste, not one line per character.
   Then paste some text with an emoji in it, such as `hi 👋 there`. It should
   arrive as one paste, with a count of 11. The sample counts UTF-16 code
   units, so the emoji counts as two.
8. **Resize the terminal window, several times, larger and smaller.** The
   status line's size and the panel layout should update straight away, without
   you pressing any key. The borders should redraw cleanly at the new size, with
   no pieces of the old frame's borders or text left anywhere on screen.
9. **Click somewhere in the terminal.** The Events panel should log a mouse
   event with the button, action, and the cell (X, Y) you clicked.
10. **Press Ctrl+E.** The program should crash with a visible error
    (`update threw on purpose (ctrl+e)`) rather than freezing on its last
    frame.
11. **Run it with input redirected**, for example
    `dotnet run --project samples/Interactive < /dev/null` on Linux or macOS.
    It should neither throw nor hang: it paints once and exits on its own,
    because it reaches the end of its input straight away. Piping the output
    instead (`| cat`) is a different case — the keyboard is still attached,
    so the program runs normally and you quit it with Esc.
12. **Linux and macOS only: kill the process from another terminal** with a
    plain `kill <pid>` while it is running. The shell should come back fully usable: your
    previous screen is back, the cursor shows, typing echoes, and moving the
    mouse over the terminal types nothing. `kill -INT <pid>` should do the
    same. `kill -9` is different: nothing can run then, so a terminal that
    does not echo afterwards is expected, and `reset` recovers it.
    On Windows, skip this item. Stopping a process from outside there —
    `taskkill /F`, or `kill` and `Stop-Process` in PowerShell — cannot be
    caught, the same as `kill -9`, so the program has no chance to restore
    anything.
13. **Press Tab, then Shift+Tab.** Tab moves the highlighted border to the
    Events panel; Shift+Tab moves it back to Typing.
14. **After every one of the exits above** (Esc, Ctrl+T timer, Ctrl+C, Ctrl+E
    crash, pipe, and `kill` on Linux and macOS) **type into the shell.** Confirm it still echoes what you
    type, that line editing (Backspace, arrow keys for history) works, and
    that Ctrl+C works normally again. This is the item that matters most — if
    the shell does not come back cleanly, raw mode was never restored, and
    none of the other checks mean anything.

## Platform-specific checks

**Linux**
- Raw mode engages on a glibc distro, and on a musl one such as Alpine.
- Ctrl+V arrives as a key.
- Started with `&` from an interactive shell, the program skips raw mode
  rather than being stopped for trying to change the terminal settings. It
  is still stopped the first time it reads the keyboard, and the shell
  prints `Stopped (tty input)`. That is how Unix treats any background job
  that reads the terminal, so it is expected.

**macOS**
- Raw mode engages. This is the check that proves the termios offsets for
  macOS are right.
- Ctrl+V, Ctrl+O and Ctrl+Y all arrive as keys.

**Windows**
- On Windows Terminal, arrows and colours work.
- `é` typed on the keyboard arrives as `é`. On an older conhost, check this
  one carefully: some builds return nothing for non-ASCII input once the input
  code page is UTF-8.

## Known limits

Not bugs — expected behaviour, left as is:

- On an older Windows console that rejects virtual-terminal input, raw mode
  still engages but arrows and function keys will not parse.
- The library's `TextInput` component types the letter for Alt+letter. This
  sample does its own typing and ignores Alt, so here Alt+letter is logged as
  a key instead.
- If raw mode fails to apply, Ctrl+C is still owned by the operating system
  and ends the process straight away. The exit hook still restores the screen
  first.
- A paste over 1 MiB arrives as several `Paste` messages, one per 1 MiB or so,
  rather than one.
