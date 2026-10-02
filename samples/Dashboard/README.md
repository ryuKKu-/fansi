# Dashboard sample

Three panels in a heavy frame. The clock takes a quarter of the width, the task
list a third, and the new-task panel the rest. It is the best test of nested
borders, mixed constraints and focus together.

    dotnet run --project samples/Dashboard

## Checklist

Run it in a terminal at least 80 columns wide.

1. Straight after launch: the timer counts down from one minute, the spinner
   turns, and the bar fills. The Clock panel has rounded corners and a cyan title.
2. Tab moves the rounded corners to New task, then to Tasks, then back to Clock.
   Shift+Tab goes the other way.
3. On Clock, Space pauses the timer: it shows ⏸ and stops. The spinner keeps
   turning. Space again resumes it.
   Once the timer reaches zero, Space starts it again from one minute.
4. On New task, the cursor blinks. Type `buy milk` and press Enter. It shows
   last in the task list, and the input empties.
5. Press Enter again on the empty input. Nothing is added.
6. Paste some text into New task. It goes into the input.
7. On Tasks, Up and Down move the marker. Enter selects a task, and the line
   under the frame shows it. Enter again clears the selection.
8. On Tasks, Space ticks "keep the text after adding". Go back to New task,
   type something, and press Enter. The text is added and stays in the input.
9. Make the terminal narrower and wider. The panels keep their shares, no
   border tears, and nothing is left behind from the old frame.
10. Make it very small, then large again. The program keeps running.
11. Esc quits, and the terminal is back as it was.
