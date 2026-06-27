# SUBCOMMANDS

## `branch`
Print the URI for accessing the currently checked out branch on GitHub.

Many operating systems have an `open` command (though the name "open" is not
ubiquitous); this means you can run something like `open $(harmony branch)` to
open a web browser to the current branch on GitHub.

## `config [property] [value]`
Read a given configuration property or show all current configuration.
`harmony config <property> <value>` will set the configuration property.

Not all configuration properties can be read/set with this command.

### Properties

`requestTeams` (`true`/`false`) 
: When picking a team to review, request review from the whole team, not just one user on the team. If you have GitHub configured to pick a member of a team to review when the team is requested, you probably want to set this to `true` so harmony tells GitHub the team from which GitHub should then pick an individual.

`requestUsers` (`true`/`false`) 
: When requesting a team to review, pick a user from the team to review as well. If you have GitHub configured to pick a member of a team to review when the team is requested, you probably want to set this to `false` so harmony does not also pick a user.

`commentOnRequest` (`none`/`name`/`at-mention`) 
: When requesting a reviewer chosen by Harmony, comment on the pull request or not.

`branchParsing` (`none`/`jira`/`github`) 
: Optionally extract a Jira ticket slug or GitHub issue number from the branch name and prepend it to the PR title or body to link the PR and ticket/issue.

`bugfixPRTitlePrefix` (optional string) 
: A string to prefix default PR titles with when the branch the PR is being created from is determined to be a bugfix branch (branch name starts with 'bugfix'). For example, a common prefix is '[fix]'.

`addPrTreeDescription` (`true`/`false`)
: Determines whether to add a tree of PRs to the description for any PR that is into a branch other than the `mainBranch` configured.

`defaultRemote` (optional string) 
: When pushing new branches, what remote destination should be used.

`mainBranch` (optional string) 
: When creating a PR, this is the default base branch.

`theme` (`dark`/`light`) 
: Use colors suited better for either a dark or light Terminal background.

`githubPAT` (optional string) 
: If the `$GITHUB_PAT` and `$GH_TOKEN` environment variables are not set, this Personal Access Token is used to authenticate with GitHub.

## `contribute [-c/--checkout | -l/--list] [-<num>] [-i/--ignore {<uri> | <pr-number>}]`
Print the URI of the oldest non-draft PR waiting for your review. If you are not
requested for review on any PRs, Harmony will suggest a PR that your review is
not requested on.

You can skip PRs and retrieve the next-oldest one by passing a dash followed by
the number to skip (e.g. `-2` to skip the two oldest waiting PRs).

You can also more permanently ignore a particular PR (perhaps it has gone
stagnant but your org does not want to close it for whatever reason). To do
this, use the `--ignore` or `-i` option and pass it the GitHub URI or the Pull
Request Number of a PR to ignore. This PR will be omitted from consideration for
the `contribute` command from then on. This only impacts your local machine
where the ignore list is stored in Harmony's config file.

You can simultaneously get the URI for a PR to review and checkout the branch
needing review by passing the `--checkout` or `-c` option to the `contribute`
command.

Many operating systems have an `open` command (though the name "open" is not
ubiquitous); this means you can run something like `open $(harmony contribute)`
to open a web browser to the PR that Harmony is suggesting.

You can also run `harmony contribute --list` if you want to list out a few PRs
to consider reviewing them instead of choosing just one PR to look into and
printing that PRs URI.

### Examples
Retrieve a URI for the oldest unreviewed and open PR (prioritizing PRs for which
you are a requested reviewer):
```shell
harmony contribute
```

Retrieve a URI for a PR to contribute a review, skipping over the first 3
suggestions:
```shell
harmony contribute -3
```

Retrieve a URI for a PR to contribute a review and check the git branch out as
well:
```shell
harmony contribute --checkout
```

Permanently ignore a PR by URI:
```shell
harmony contribute --ignore https://github.com/myorg/myrepo/pull/1234
```

Permanently ignore a PR by its number:
```shell
harmony contribute --ignore 1234
```

## `graph [--completed] {team-slug}`
Graph the relative review workload of each of the members of the given GitHub
Team.

You can optionally graph completed PR reviews with the `--completed` flag as
well, though these are not considered for Harmony's weighting algorithm for
review workload.

## `health`
Graph all open PRs grouped by the month when each was created.

The idea is that a healthy repository does not have many old PRs still open
because those PRs represent effort spent by developers that hasn't yet paid off.

<!-- image location is intentionally relative to repository root -->
![Health Screenshot](./docs/images/health.png)

## `help [subcommand]`
Print help.

## `label {label} [...]`
Helps you create a PR if one does not exist yet and then it will apply the given
labels to the PR. This is essentially an alias for the `harmony pr` command but
without support for creating draft PRs.

Note that labels are _not_ prefixed with '#' for this command. There is no need
to differentiate labels from other kinds of arguments to `harmony label`.

## `list [team-slug]`
Running `harmony list` will list all the teams for the configured GitHub
organization.

Running `harmony list <team>` will list the members of the given GitHub Team.

## `pr [--draft | --ready] [--issue] [--project {<project-number> | <project-title>}] [-i/--into {<branch-name>}] [-o/--output {format}] [--print-tree] [#label, ...]`
With a branch checked out will reach out to GitHub to determine if there is an
open PR for that branch. If there is a PR, Harmony will print a URI that can be
used to view the PR. If there is not a PR, Harmony will help you create one. The
`--print-tree` flag will print a tree of PRs for branches between the current
one and the `mainBranch` of the repository and between the current PR and any
downstream PRs instead of just printing the URI for the current branch's PR. New
and existing PRs can be marked as drafts by specifying the `--draft` flag with
the `pr` command or they can be marked as ready for review with the `--ready`
flag. The default behavior for new PRs is to mark them ready.

When creating a new PR interactively, `--issue` creates a GitHub issue first,
uses its title as the default PR title, and adds the usual `Related to #N` link
to the PR body. `--issue` only works for new PRs; existing PRs need issue
association handled separately for now.

When creating a new issue (see `--issue`), the `--project` option can be used to
associate the new issue with an existing project.

By default this command outputs to a shell format that is colored (if supported)
but you can use the `--output markdown` option to output to a markdown syntax
instead.

If you need to create a PR still, you will be prompted for a branch to open the
PR against (merge into, eventually), a title for the PR, and a description for
the PR. If you have an `EDITOR` environment variable set, Harmony will use that
editor to get the PR description from you. If you have a PR template at
`.github/PULL_REQUEST_TEMPLATE.md`, Harmony will also preload that into your
editor. If you do not have an `EDITOR` environment variable set, you will still
be able to enter a description from the command line but PR templates are only
supported when an `EDITOR` is specified.

If you are creating a new PR from a branch that refers to a GitHub issue (and
you have GitHub branch parsing enabled in your harmony config) then harmony will
prepend the issue information onto your new PR description in a commented out
block for you to reference or copy into the PR description as needed.

You can specify the branch to merge into via the `--into` CLI argument if you
want to as an alternative to the interactive prompt.

You can also specify any number of labels to apply by prefixing them with '#'.
For example, `harmony pr \#backport \#bugfix` would create a PR and apply the
`backport` and `bugfix` labels. Note that some shells will require you to escape
the `#` or else it will be considered a comment and not passed to harmony as an
argument.

If you are using harmony from a script or some other environment without TTY
support, harmony will print a GitHub URL that can be used to create the PR. This
mode of operation will ignore the `--draft` and `#label` options. `--issue` is
interactive-only because it prompts for issue details.

Many operating systems have an `open` command (though the name "open" is not
ubiquitous); this means you can run something like `open $(harmony pr)` to open
a web browser to an existing PR for the current branch.

### Examples
Create a draft pull request for the current branch:
```shell
harmony pr --draft
```

Create a PR for the current branch and add the `urgent` label:
```shell
harmony pr \#urgent
```

Create a pull request that will merge into the hypothetical pre-existing
`release/2_0` branch:
```shell
harmony pr --into release/2_0
```

Create a GitHub issue and then create a PR linked to that issue:
```shell
harmony pr --issue
```

## `quick [--bugfix] [--project {<project-number> | <project-title>}] [issue-title | #<issue-number>] [...]`
Helps you create a new GitHub issue and a branch to work on that issue all in
one go. The branch name will be structured such that if you have GitHub branch
parsing on then the PR you create for the branch later on will refer to the
issue created now.

By default the branch created will be prefixed with `feature` but if you specify
the `--bugfix` flag then the branch's prefix will be `bugfix`.

By default the issue created will not be associated with any projects. If you
specify the `--project` option with either a project number or project title
then that project will be looked up and associated with the new issue. You
cannot create new projects this way; only existing projects are supported.

If your only argument (possibly in addition to the bugfix flag) is
`#<issue-number>` for an existing GitHub issue then the new branch will point at
that existing issue. Otherwise, all additional arguments other than `--bugfix`
and `--project {ref}` will be used as the issue title. If you don't specify a
title at the CLI then you will be prompted to enter the issue title
interactively. You will also be prompted for the issue description interactively
regardless.

### Examples
Create an issue and bugfix branch:
```shell
harmony quick --bugfix
```

Create an issue with the title 'widget fails to create' and bugfix branch:
```shell
harmony quick --bugfix widget fails to create
```

Create a branch for the existing issue with GitHub issue number `345`:
```shell
harmony quick \#345
```

Create an issue (and branch) and associate it with the 'Cool Features' project:
```shell
harmony quick --project "Cool Features"
```

## `reflect`
Show a summary of your review requests and authored pull requests.

<!-- image location is intentionally relative to repository root -->
![Reflect Screenshot](./docs/images/reflect.png)

## `request {<team-slug> | +<user-login>} [#<label>] [...]`
Helps you create a PR if one does not exist yet and then it will request reviews
from teams and/or users.

There is also a `harmony rq` alias for `harmony request`.

If `harmony config requestUsers` is `True` (defualt) then harmony will pick
someone to review the PR (from one of the listed teams). If `harmony config
requestTeams` is `True` (default) then harmony will request reviews from the
teams you listed. If `harmony config commentOnRequest` is `True` then harmony
will comment on the Pull Request indicating that teams & users were
"harmoniously requested" -- this comment will @mention requested users so it may
be useful or annoying depending on the requested user's GitHub notification
settings.

You can also require that specific additional users (on top of the one Harmony
will pick for you) are requested to review the PR. You do this by specifying
those users' logins prefixed with '+' as arguments to Harmony. This will request
review from those specific additional users regardless of the `requestUsers`
setting; that setting controls whether Harmony picks users from each Team you
specify to review PRs.

You can optionally apply any number of labels to the PR at the same time as
requesting reviewers by prefixing the labels with '#'.

### Deferring to GitHub
If your team has GitHub set up to auto-request reviews from individuals when a
team is requested for review, you probably want to tell harmony not to also pick
someone using its heuristics. You can run the following `config` commands to
tell harmony to request a team but not also pick an individual from that team:
```shell
harmony config requestTeams true
harmony config requestUsers false
```
This does not prevent you from requesting specific individuals with the
`+<user>` syntax described above.

### Examples
Request review from the most available reviewer from the "developers" GitHub
Team:
```shell
harmony request developers
```

Request review from the most available reviewer from either the "frontend" or
"backend" GitHub Team:
```shell
harmony request frontend backend
```

Request review from the most available reviewer from the "web" team and
additionally request review from the users with logins "carl001" and "emmaham":
```shell
harmony request web +carl001 +emmaham
```

## `sync`
Sync the locally configured team slugs and user logins that are used by
auto-completion for Harmony. This sync is also performed automatically the first
time you run Harmony after more than a day without the configuration being
synced.

## `version [-s/--short]`
Print Harmony's version.

To print a shortened output more useful from scripts, pass `-s` or `--short`.

## `whoami`
Print information about the currently configured and authenticated user.

