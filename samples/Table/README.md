# Table sample

A table of made-up processes with a cursor row, and a help line built from
the sample's own key bindings.

The component logic has unit tests. What they cannot check is how it looks
and feels in a real terminal. This checklist is for that.

## Run it

    dotnet run --project samples/Table

## Acceptance checklist

1. **Columns.** The table has a single-line border with junctions. Name fills the room left by CPU, Memory and Status. The
   header is bold. Status is green for running processes.
2. **Cursor.** Up and Down move the cyan row, PageUp and PageDown a page,
   Home and End to the first and last process. The status line follows.
3. **Scrolling.** Move past the bottom of the screen. The table scrolls just
   enough to keep the cursor in view, and never past the last row.
4. **Wheel.** The mouse wheel moves the cursor one row per notch.
5. **Wide names.** `日本語-agent` and `📦-cache` line up with the other names.
   Nothing after them shifts.
6. **Help.** The bottom line lists the keys. `?` switches to full help in two
   columns, and `?` again switches back.
7. **Resize.** Make the window narrower and shorter. The Name column shrinks
   first, the help line drops whole entries and ends with `…`, and the cursor
   stays in view.
8. **After Esc, type into the shell.** It echoes and line editing works.
