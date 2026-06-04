## Usage
Once configured, Harmony supports the following commands that are documented
below: `config`, `branch`, `pr`, `quick`, `label`, `request` (also aliased to
`rq`), `contribute`, `whoami`, `reflect`, `list`, `graph`, `health`, and `sync`.

Some of Harmony's features will request you to enter titles, descriptions, etc. at the CLI. You can walk through these prompts and enter values interactively from the shell, but the experience of entering longer descriptions via a text editor is much more pleasant. Make sure you have the `$EDITOR` environment variable set to your preferred editor if you want Harmony to redirect you to the editor to enter longer text values. This is highly recommended.

**Example `$EDITOR` Values:**

- Vim: `export EDITOR=vim`
- VS Code: `export EDITOR='code --new-window --wait'`

**Note on color output:**
Harmony uses colored output for some commands. You can adjust these colors
slightly with the `theme` configuration option. You can also use the `NO_COLOR`
environment variable to disable all colored output. Lastly, Harmony will avoid
colored output when it determines `stdout` is not a TTY device (as is the case
for e.g. redirecting harmony output into a file or piping into `cat`: 
`harmony ... | cat`).

