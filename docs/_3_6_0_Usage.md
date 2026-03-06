## Usage
Once configured, Harmony supports the following commands that are documented
below: `config`, `branch`, `pr`, `quick`, `label`, `request` (also aliased to
`rq`), `contribute`, `whoami`, `reflect`, `list`, `graph`, `health`, and `sync`.

**Note on color output:**
Harmony uses colored output for some commands. You can adjust these colors
slightly with the `theme` configuration option. You can also use the `NO_COLOR`
environment variable to disable all colored output. Lastly, Harmony will avoid
colored output when it determines `stdout` is not a TTY device (as is the case
for e.g. redirecting harmony output into a file or piping into `cat`: 
`harmony ... | cat`).

