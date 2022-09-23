Guile Script Format
===================

### `%summary`
String containing a summary of what the module does.
Should be a single line.

### `%include-in-guild-list`
Boolean, indicating if the script should be listed when running `guild help` or `guild list`.

### `%help`
Longer help for module. If this variable isn't set the procedure `module-commentary` is run

### `%synopsis`
Short help showing how to invoke the script. Should *not* include the guild command.

### `main`
Procedure which is primary entry point. Gets remaining command line as its arguments (meaning it takes multiple arguments).
