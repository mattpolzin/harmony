## Usage
The first time you start Harmony in any particular folder, you will be asked to
provide some information about the GitHub repository you are working with. This
information is stored in a file named `harmony.json` in the current working
directory.

Note that the GitHub organization and repository are both slugs, not names.
These are the values you find in a GitHub URL pointing to your repository.
Harmony works with personal repositories but some of Harmony's features are not
available for these repos because they do not have teams or members.
```
$ harmony sync
Creating a new configuration (storing in harmony.json)...

Harmony uses a GitHub Personal Access Token (PAT) to communicate with GitHub.
You can set this via the $GITHUB_PAT or $GH_TOKEN environment variables or a config property.
If you don't set in your config now, you can set later with `harmony config githubPAT abcdefg`.
The ENV var will always take precedence over the config property.

What PAT would you like to set in the config file (ENTER for default: unset)?

What GitHub org would you like to use harmony for (ENTER for default: myorg)?

What repository would you like to use harmony for (ENTER for default: myrepo)?

What GitHub remote repo would you like to use harmony for (ENTER for default: origin)?

Would you like harmony to comment when it requests reviewers? [Y/n] 
Would you like harmony to request team reviews in addition to individuals when it requests reviewers? [Y/n] 
Creating config...
```

Once configured, Harmony supports the following commands: `config`, `branch`,
`pr`, `quick`, `label`, `request` (also aliased to `rq`), `contribute`,
`whoami`, `reflect`, `list`, `graph`, `health`, and `sync`.

**Note on color output:**
Harmony uses colored output for some commands. You can adjust these colors
slightly with the `theme` configuration option. You can also use the `NO_COLOR`
environment variable to disable all colored output. Lastly, Harmony will avoid
colored output when it determines `stdout` is not a TTY device (as is the case
for e.g. redirecting harmony output into a file or piping into `cat`: 
`harmony ... | cat`).

