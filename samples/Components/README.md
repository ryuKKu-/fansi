# Components sample

Every Fansi component on one screen: two text inputs, a list, a checkbox, a
button, a timer, a spinner and a progress bar. One focus ring drives them all.

The component logic has unit tests. What they cannot check is how it looks and
feels in a real terminal. This checklist is for that.

## Run it

```
dotnet run --project samples/Components
```

## Keys

- Tab and Shift+Tab move focus. The focused box has a rounded border.
- In the name field, Tab completes a suggestion when one shows.
- Ctrl+S pauses and restarts the timer.
- Esc quits.

## Acceptance checklist

Do item 1 first, straight after launching the sample: the timer only runs
for 30 seconds, and the rest of the checklist takes longer than that. If you
run out of time, restart the sample to reset the timer and come back to
item 1.

1. **Things that move on their own.** Without pressing a key, the spinner
   spins, the timer counts down and the progress bar fills. Ctrl+S pauses the
   timer and the bar. When the timer reaches zero, the status shows
   `time is up`.
2. **Only the focused input blinks.** Tab between the name and password
   fields. The cursor blinks in the focused one and does not show in the
   other.
3. **The name field scrolls.** Type more than 20 characters. The text scrolls
   left and the cursor stays in view. Home jumps to the start, End to the end,
   and the cursor is visible at both.
4. **Word keys.** With a few words typed: Ctrl+Left and Ctrl+Right, then Alt+B
   and Alt+F, jump a word at a time. Ctrl+W deletes the word before the
   cursor, Alt+D the word after it. Ctrl+K deletes to the end, Ctrl+U to the
   start. Ctrl+Backspace deletes a word on terminals that send a distinct byte
   for it. Note in the report whether yours does.
5. **Undo.** Type a word, then press Ctrl+Z once. The whole word goes. Ctrl+Y
   brings it back.
6. **Suggestions.** Clear the name, then type `gr`. `ace Hopper` shows dimmed
   after the cursor, but only while the cursor sits at the end of what you
   typed. Tab completes it to `Grace Hopper`. Clear the field again (Ctrl+U
   from the end), type `a`, then press Ctrl+N and Ctrl+P to cycle between Ada
   and Alan.
7. **Validation.** The name starts empty, so `a name is required` shows in
   red under the field from the start. Type a letter and it goes. Delete
   everything and it comes back.
8. **Password.** In the password field, type anything. Only `*` shows. After
   16 characters, typing stops. Clear the field (Ctrl+U), then type `se`: no
   suggestion shows, because a password field never shows one, even though
   the pool holds `secret-password`.
9. **Alt+letter.** In either field, Alt+Q types nothing.
10. **Paste.** Paste several lines of text into the name field. It arrives on
    one line, with the line breaks and any other control characters gone. One
    Ctrl+Z removes the whole paste. Clear the password field (Ctrl+U), then
    paste 30 characters into it: 16 go in. To see that an empty paste still
    breaks a run of typing into its own undo step: clear the name field, type
    `ab`, paste an empty line (so nothing is inserted), type `cd`, then press
    Ctrl+Z once. The field goes back to `ab`, not to empty — the paste ended
    the run `ab` was typed in, so the one Ctrl+Z only undoes `cd`.
11. **Emoji.** Paste `hi 👋` into the name field. Left steps over the emoji
    in one press, and Backspace removes it in one press. No stray `?` or box
    is left behind.
12. **List.** Up and Down move the marker. The list scrolls once you pass the
    third item. Enter selects the marked item; Enter again clears it.
13. **Checkbox and button.** Space or Enter toggles the checkbox. Enter on the
    button writes a status line with the name, the password length, the fruit
    and the checkbox state.
14. **Resize.** Make the window smaller and larger. The layout redraws with no
    broken borders.
15. **After Esc, type into the shell.** It echoes and line editing works.
