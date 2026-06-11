# 6.10.1 (The open road)
Published: 2026-06-11T01:47:10Z

## What's Changed
* Short help (https://github.com/mattpolzin/harmony/pull/310)
* make "you are here" text in PR description trees more visible (https://github.com/mattpolzin/harmony/pull/312)
* add an author section to the manpage (https://github.com/mattpolzin/harmony/pull/313)
* [fix] PR Tree includes closed PRs (https://github.com/mattpolzin/harmony/pull/316)


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.10.0...6.10.1


You can build the source with the latest `pack` packageset or `nix`.

# 6.10.0 (Here's the issue with that)
Published: 2026-06-08T02:04:55Z

## What's Changed
* Creates issue alongside pr by @TimDurward in https://github.com/mattpolzin/harmony/pull/305

## New Contributors
* @TimDurward made their first contribution in https://github.com/mattpolzin/harmony/pull/305

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.9.1...6.10.0


You can build the source with the latest `pack` packageset or `nix`.

# 6.9.1 (Don't tell me what to do. I know what to do)
Published: 2026-06-04T15:10:00Z

## What's Changed
* omit branch annotation if main branch (https://github.com/mattpolzin/harmony/pull/302)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.9.0...6.9.1


You can build the source with the latest `pack` packageset or `nix`.

# 6.9.0 (Look Both Ways)
Published: 2026-06-04T01:45:43Z

## What's Changed
* when printing the PR tree for the current branch, look up as well as down (https://github.com/mattpolzin/harmony/pull/298)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.8.0...6.9.0


You can build the source with the latest `pack` packageset or `nix`.

# 6.8.0 (Based)
Published: 2026-05-30T18:51:39Z

## What's Changed
* store base branch for quick issues (https://github.com/mattpolzin/harmony/pull/295)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.7.0...6.8.0


You can build the source with the latest `pack` packageset or `nix`.

# 6.7.0 (Mark me down for Matt)
Published: 2026-05-28T01:26:14Z

## What's Changed
* Switch off of deprecated issues api (https://github.com/mattpolzin/harmony/pull/287)
* add a `--output` format option to the pr command (https://github.com/mattpolzin/harmony/pull/293)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.6.0...6.7.0


You can build the source with the latest `pack` packageset or `nix`.

# 6.6.0 (Look mom, no hands)
Published: 2026-05-10T17:52:58Z

## What's Changed
* add shell completion for settable configs that accept an enumeration of values (https://github.com/mattpolzin/harmony/pull/280)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.5.0...6.6.0


You can build the source with the latest `pack` packageset or `nix`.

# 6.5.0 (Pack it up)
Published: 2026-05-06T02:01:20Z

## What's Changed
* Switch to using pack for non-nix builds
* bugfix prefix (https://github.com/mattpolzin/harmony/pull/277)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.4.0...6.5.0


You can build the source with the latest `pack` packageset or `nix`.

# 6.4.0 (To The Center Of The Earth)
Published: 2026-05-02T03:12:30Z

## What's Changed
* PR trees (https://github.com/mattpolzin/harmony/pull/263)
 
**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.3.1...6.4.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 6.3.1 (Quick and precise)
Published: 2026-01-27T02:39:24Z

## What's Changed
* make quick only issues (https://github.com/mattpolzin/harmony/pull/253)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.3.0...6.3.1


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 6.3.0 (Support existing issues for `quick` command)
Published: 2026-01-24T18:54:10Z

## What's Changed
* quick command can be used with just an issue number by @mattpolzin in https://github.com/mattpolzin/harmony/pull/235
* Fix crash after marking PR ready by @mattpolzin in https://github.com/mattpolzin/harmony/pull/247

`harmony quick \#1234` is now short-hand for creating a branch locally for the given issue. This doesn't do much beyond `git checkout -b feature/1234/whatever` but it does allow you to accept-by-default a branch name built from the issue title. So if the issue title is "Fix a bug" then you can quickly and with few key-presses get a branch named `bugfix/1234/fix-a-bug` via `harmony quick --bugfix \#1234`.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.2.0...6.3.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 6.2.0 (More defaults)
Published: 2026-01-19T03:42:49Z

## What's Changed
* Add better PR title handling to `pr` command (https://github.com/mattpolzin/harmony/pull/225)
* Add enter-for-default option to `quick` command for the branch name (https://github.com/mattpolzin/harmony/pull/227)


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.1.1...6.2.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 6.1.1 (Overflowing with stability)
Published: 2026-01-07T04:13:08Z

## What's Changed
* Fix a stack overflow from iterating over characters in large strings recursively.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.1.0...6.1.1


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 6.1.0 (You complete me)
Published: 2026-01-06T15:44:08Z

## What's Changed
* ZSH shell completion now offers up descriptions of possible completion results.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/6.0.0...6.1.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 6.0.0 (Ready or Not)
Published: 2025-12-25T05:13:50Z

## What's Changed
* No more dependence on the `simple-git` NodeJS package.
* Auto-completion for the `pr` command has been fixed (https://github.com/mattpolzin/harmony/pull/209)
* Adds a `--ready` flag for the `pr` command that marks a draft PR ready for review (https://github.com/mattpolzin/harmony/pull/209)

## Breaking
The `theme` harmony.json configuration option is now required. If you have run `harmony` at any `5.x` version then your config will have been automatically updated with this option set to `dark` by default. You can run `harmony` at `5.8.0` to update a `v4.x` config automatically or you can manually add `"theme": "dark"` to your harmony.json.

The `commentOnRequest` harmony.json configuration option can no longer be a boolean (`true`/`false`) value. If you have run `harmony` at any `5.x` version then your config will have been automatically updated with this option set to `"none"` if it was previously `false` or `"at-mention"` if it was previously `true`. You can run `harmony` at `5.8.0` to update a `v4.x` config automatically or you can manually update an old boolean value yourself -- valid options are: `"none"`, `"name"`, or `"at-mention"`.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.8.0...6.0.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.8.0 (Even Quicker)
Published: 2025-12-15T03:36:01Z

## What's Changed
* Fix autocomplete of bugfix flag for quick command (https://github.com/mattpolzin/harmony/pull/195)
* support taking quick title as CLI args (https://github.com/mattpolzin/harmony/pull/198)


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.7.0...5.8.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.7.0 (Didn't we go over this already?)
Published: 2025-12-08T03:50:45Z

## What's Changed
* Populate PR descriptions with GitHub issue context when available (https://github.com/mattpolzin/harmony/pull/193)


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.6.0...5.7.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.6.0 (Make that a quick-fix)
Published: 2025-11-27T03:06:01Z

## What's Changed
* Add `--bugfix` flag to `quick` command (https://github.com/mattpolzin/harmony/pull/186)


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.5.1...5.6.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.5.1 (In Great Detail)
Published: 2025-11-26T04:09:17Z

## What's Changed
* Add manpage to npm package by @mattpolzin in https://github.com/mattpolzin/harmony/pull/181
* Support `EDITOR` for issue description from quick command by @mattpolzin in https://github.com/mattpolzin/harmony/pull/182


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.5.0...5.5.1


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.5.0 (Quick)
Published: 2025-11-17T17:24:38Z

## What's Changed
* Better spacing between prompts for initial config setup (https://github.com/mattpolzin/harmony/pull/174)
* Add new quick command (https://github.com/mattpolzin/harmony/pull/176)

The new `harmony quick` command will prompt for details for a new GitHub issue and then create and check out a feature branch that references the new ticket number in its 'path' so that subsequent PR creation will refer back to the GitHub issue in the PR description automatically if you have Harmony configured to parse GitHub issue numbers in branches (`harmony config branchParsing github`).


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.4.2...5.5.0


You can build the source with Idris 2 `v0.8.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.4.2 (Parsing and PR template newline fixes)
Published: 2025-08-25T01:26:28Z

## What's Changed
* Fix GitHub parsing and newlines in PR templates by @mattpolzin in https://github.com/mattpolzin/harmony/pull/173


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.4.1...5.4.2


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.4.1 (More robust GitHub issue number parsing for branch names)
Published: 2025-08-24T04:25:43Z

## What's Changed
* More github branch parsing support by @mattpolzin in https://github.com/mattpolzin/harmony/pull/171


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.4.0...5.4.1


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.4.0 (New branch parsing configuration options)
Published: 2025-08-24T03:55:30Z

## High Level
Harmony used to look for Jira slugs in branch names and automatically prefix new PR titles with the Jira Slug. For example, the branch `feature/SUP-1234/fix-thing` would result in a PR title prefixed with `SUP-1234`.

As of this version, that remains the default for backwards compatibilty. You now have the option to switch this off or change it to look for GitHub issue numbers instead. The latter means the branch name `feature/1234/fix-thing` results in a PR description starting with `Related to #1234` (which tells GitHub to link the PR to that issue number).

## What's Changed
* Move the completed graph to the right of user logins by @mattpolzin in https://github.com/mattpolzin/harmony/pull/162
* add config option to turn off jira slug parsing in branch names by @mattpolzin in https://github.com/mattpolzin/harmony/pull/163
* support parsing github issues from branch names by @mattpolzin in https://github.com/mattpolzin/harmony/pull/169


**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.3.1...5.4.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.3.1 (Fix bug with labels for `pr` command)
Published: 2025-07-09T02:43:08Z

## What's Changed
* Fix bug with labels for the `pr` command (https://github.com/mattpolzin/harmony/pull/160)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.3.0...5.3.1


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.3.0 (Auth and PR creation improvements)
Published: 2025-07-08T16:14:17Z

## What's Changed
* Add `--into` option to `pr` command (https://github.com/mattpolzin/harmony/pull/154)
* Fix crash if creating a PR and pushing a conflicting commit (https://github.com/mattpolzin/harmony/pull/155)
* Support `GH_TOKEN` in addition to `GITHUB_PAT` (https://github.com/mattpolzin/harmony/pull/156)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.2.0...5.3.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.2.0 (Happier Scripting)
Published: 2025-04-28T02:47:25Z

## What's Changed
You can now use `harmony pr` to print a GitHub URL that will create a new PR when harmony recognizes that the shell it is running in is not a TTY shell. This change won't impact the most common CLI use cases but if you want to use `harmony pr` from a script this may come in handy.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.1.0...5.2.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.1.0 (Draft any time)
Published: 2025-03-07T22:47:26Z

## What's Changed
You can now use `harmony pr --draft` to convert an existing PR into a draft. Previously the `--draft` flag was only used when creating new PRs.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/5.0.0...5.1.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 5.0.0 (Just give me the deets)
Published: 2024-07-05T23:17:03Z

## What's Changed
With this version, you can now configure Harmony's comment when it requests review based on its heuristic. Previously, the comment was either enabled (with an at-mention to the requested user) or disabled. Now you can disable it, or enable it with an at-mention to the requested user _or_ using the requested user's name. The new comment with name option may be preferable for some organizations because there is no notification tied to the comment itself like there would be with a mention.

`harmony config commentOnRequest none` is the same as `harmony config commentOnRequest no` used to be.

`harmony config commentOnRequest at-mention` is the same as `harmony config commentOnRequest yes` used to be.

`harmony config commentOnRequest name` is the the new setting to comment and use  the requested user's name instead of mentioning them.

This version also adds support for `harmony contribute --list` which lists a number of PRs you may want to review instead of only operating on one PR at a time.

### Non-breaking
* Comment strategy config (https://github.com/mattpolzin/harmony/pull/135)
* Add support for listing PRs in need of review (https://github.com/mattpolzin/harmony/pull/134)

### Breaking
* Support for the deprecated `assign` command has been removed. Use the equivalent `request` (or `rq`) command instead. This command was renamed around the time of the previous major release.
* Support for the following deprecated configuration options has been removed:
  * `assignUsers` (renamed `requestUsers`)
  * `assignTeams` (renamed `requestTeams`)
  * `commentOnAssign` (renamed `commentOnRequest`).

If you are coming from a Harmony version prior to 4.0.0, the easiest way to update is to install Harmony 4.4.0, run `harmony sync`, and then install Harmony 5.x. This will automatically migrate your configuration file(s). Alternatively, you can manually edit `harmony.json` to rename the above configuration keys.

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/4.4.0...5.0.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 4.4.0 (Maybe not every project is Org-anized)
Published: 2024-07-02T04:11:40Z

## What's Changed
### Non-breaking
* Allow much of harmony to be used with non-org github repos (https://github.com/mattpolzin/harmony/pull/132)
* list teams when list command is not given a team name (https://github.com/mattpolzin/harmony/pull/133)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/4.3.0...4.4.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 4.3.0 (Add a `light` theme configuration option)
Published: 2024-06-30T16:19:12Z

## What's Changed
### Non-breaking
* read terminal width, make help and other output reflow to that width (https://github.com/mattpolzin/harmony/pull/127)
* Light theme (https://github.com/mattpolzin/harmony/pull/131)

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/4.2.0...4.3.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 4.2.0 (Support setting `mainBranch` config from CLI)
Published: 2024-05-31T03:59:51Z

Add ability to set `mainBranch` config prop via CLI

# 4.1.0 (New `rq` alias for the `request` command)
Published: 2024-03-08T16:02:20Z

## Changes

### Non-breaking
- Add `rq` alias for `request` command in https://github.com/mattpolzin/harmony/pull/121

**Full Changelog**: https://github.com/mattpolzin/harmony/compare/4.0.1...4.1.0


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 4.0.1 (Add missing `help` output)
Published: 2024-01-22T05:24:55Z

## Changes

### Non-breaking
- Added missing `help` command documentation for the now-deprecated `assign` command.


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 4.0.0 (Rename `assign` command to `request`)
Published: 2024-01-15T23:48:22Z

## Changes
The way harmony used the word "assign" in commands, configuration options, and help text was often not aligned with the way GitHub uses that same word. Where Harmony used to say "assigned a reviewer" GitHub would say "requested a review;" since GitHub also has a totally distinct concept of assigning (assigning users to Pull Requests), this was regrettable naming for the Harmony CLI. I've changed the Harmony word choice going forward, though the commands and configuration options from prior versions of Harmony will continue to work for the duration of the `4.x` releases.

### Non-breaking
- The `assign` command has been renamed to `request`; `assign` will continue to work with a deprecation warning for the duration of the `4.x` releases in order to ease the transition to using the new name.
- The `assignTeams` and `assignUsers` configuration options were renamed `requestTeams` and `requestUsers` respectively. The old config names will continue to work for the duration of the `4.x` releases to ease the transition to the new names.

### Breaking
- The `assignTeams` and `assignUsers` config options in the `harmony.json` configuration file were renamed to `requestTeams` and `requestUsers`. Although the old names will continue to work via the `harmony config ...` CLI command, only the new names will be stored in the `harmony.json` file. This makes the configuration file not backwards compatible with older versions of Harmony. Harmony will automatically update the config file for you.


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 3.3.0 (Improve Help)
Published: 2024-01-15T23:07:48Z

## Changes
### Non-breaking
- You can now run `help <subcommand>` to get help for a specific subcommand.
- The full `help` command output is now sorted by subcommand alphabetically.
- The `help` description for shell prompt completion has been updated to reflect the newest way to use Zsh completion for Harmony.


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 3.2.0 (More graph command context printed to screen)
Published: 2024-01-08T16:09:10Z

## Changes
### Non-breaking
- I chose to add an additional note at the bottom of the `graph` command output that explains my strong opinion about what the graph output does and does not represent.


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 3.1.0 (Add more direct zsh completion support)
Published: 2023-12-30T22:16:05Z

## Changes
### Non-breaking
- Support `zsh` completion more directly. See the [README](https://github.com/mattpolzin/harmony/blob/main/README.md#zsh-completion).


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 3.0.0 (With great speed comes great greatness)
Published: 2023-12-27T16:40:27Z

## Changes
### Breaking
- At build-time, requires Idris2 v0.7.0.
- At runtime, requires NodeJS 18+.
- Drops backwards compatibility for several config options that were introduced during the 2.x period. These config changes migrate themselves when you use any Harmony command, but if you are at all concerned about this breaking change, make sure you run `harmony sync` once with Harmony version 2.6.2 before upgrading to Harmony 3.0.0.

### Non-breaking
- Implements faster JSON parsing which speeds up several of the slower Harmony commands.


You can build the source with Idris 2 `v0.7.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.6.2 (Skip Unhelpful Contribute Recommendations)
Published: 2023-12-06T15:59:26Z

## Changes
When requesting a PR to contribute a review to, draft PRs will be skipped over. This way, the PR Harmony suggests is actually going to be ready for your review.


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.6.1 (Fix Github Team Assignments)
Published: 2023-08-19T03:59:37Z

## Changes
_non-breaking:_
Teams are assigned prior to individuals which allows GitHub to assign individuals from the team via round-robin or weight-based approaches if it is configured to do so. If GitHub is not configured to do so, this has no impact on Harmony's functionality.


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.6.0 (Additional prompts during PR creation)
Published: 2023-07-27T15:55:14Z

## Changes
_non-breaking:_
Two new prompts that only show up when relevant to make sure that you are ready to create a PR when you go to create one:
1. Do you have unstaged uncommitted changes locally?
2. Do you have staged uncommitted changes locally?

I think a future iteration will swap the second prompt with the ability to create a commit or else it would make less sense to separate the two out like this.


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.5.0 (Add `--ignore` option to `contribute` command.)
Published: 2023-07-03T15:35:57Z

## Changes
_non-breaking:_
- Add `--ignore <uri>/<pr-num>` option to `contribute` command. Use this option to permanently ignore (local to your machine only) a particular PR when delivering results for the `contribute` command.


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.4.1 (Fix label auto completion and application)
Published: 2023-06-30T16:45:26Z

## Changes
_non-breaking:_
- Fix label auto complete for the `assign` command.
- Fix label application for the `pr` command (no longer applies label with the `#` prefix in place).


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.4.0 (Add labelling support to `pr` command)
Published: 2023-06-28T16:32:06Z

## Changes
_non-breaking:_
- Add support for specifying labels to add to a PR via the `harmony pr` command.


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.3.0 (Add `health` command)
Published: 2023-01-30T02:17:54Z

## Changes
_non-breaking:_
- Add a new `health` command that shows all open Pull Requests grouped by the month when the PR was created.


You can build the source with Idris 2 `v0.6.0` _or_ the latest HEAD of the main Idris 2 branch.

# 2.2.0 (Add `--draft` flag to `pr` command)
Published: 2022-12-04T22:48:20Z

## Changes
_non-breaking:_
- Add`--draft` flag to `pr` command to facilitate creating draft PRs. Converting an existing PR to a draft is not yet supported.


You can build the source with Idris 2 `v0.6.0`.

# 2.1.0 (Add ability to specify labels with the `assign` command)
Published: 2022-12-04T21:48:26Z

## Changes
_non-breaking:_
- Allow specifying labels to apply when using the `assign` command. In this context, labels are specified with a `#` prefix to differentiate them from team names and user logins.


You can build the source with Idris 2 `v0.6.0`.

# 2.0.0 (Add `label` command to add labels to a PR)
Published: 2022-12-04T17:23:11Z

## Changes
_breaking_:
- The minimum NodeJS requirement changed from v12 to v14.
- Tab completion (bash completion) is not backwards compatible so you'll need to restart your shell, re-source your rc file, or re-run `eval "$(harmony --bash-completion-script)"` after updating from v1.x if you are using tab completion.

_non-breaking:_
- Add a `label` command that adds one or more labels to a PR (and creates a PR if needed).

### Notes
ℹ️ Harmony syncs its cache of users, teams, and labels once per day (when you run it). If you've run harmony in the past 24 hours before updating from v1.x to v2.x then you should run `harmony sync` manually to populate your local cache of labels so that tab completion for the `label` command works properly.


You can build the source with Idris 2 `v0.6.0`.

# 1.3.0 (Add `--completed` flag for `graph` command)
Published: 2022-11-01T04:44:13Z

## Changes
_non-breaking:_
- Support graphing completed PR reviews with the new `--completed` flag for the `graph` command.

### Notes
ℹ️ Harmony does not use completed PR reviews in calculating its weighted review workload; graphing completed PR reviews is just available to give the user a bigger picture.
ℹ️ This is the first release to require Idris 2 `v0.6.0` -- The source code is no longer compatible with the `v0.5.1` release of Idris 2.


You can build the source with Idris 2 `v0.6.0`.

# 1.2.0 (New `assignUsers` configuration option)
Published: 2022-09-11T03:46:49Z

Harmony has always assigned users from teams because the biggest reason for Harmony existing is to supply a heuristic upon which such assignments can be made.

However, GitHub also supports two styles of automated review request: round robin and a balancing heuristic. The new config option that allows turning off Harmony's user assignment feature allows Harmony to be used with GitHub's automatic assignment because Harmony will provide a CLI for creating PRs and assigning teams after which GitHub will take over and assign individuals.

When this is desirable, Harmony definitely does less but it still surfaces a reasonable CLI to PR creation and team assignment, good auto-completion, and its reflect command is still useful.


You can build the source with Idris 2 `v0.5.1`

# 1.1.1 (NPM Readme)
Published: 2022-07-24T17:25:03Z

## Changes
N/A

### Notes
ℹ️ Going forward, the Harmony README will be packaged with NPM releases.


You can build the source with Idris 2 `v0.5.1`

# 1.1.0 (Alt. PAT)
Published: 2022-07-23T20:02:31Z

## Changes
_non-breaking:_
- Allow the GitHub Personal Access Token to be specified in the config file under the `githubPAT` option as an alternative to setting it via the `$GITHUB_PAT` environment variable.

### Notes
ℹ️ A GitHub Personal Access Token is still required. If the `$GITHUB_PAT` environment variable is set, that value will always be used. If no environment variable is set but the `githubPAT` option in the config file is set, that value will be used. You can set the config value with `harmony config githubPAT abcde` where `abcde` is your Personal Access Token.  


You can build the source with Idris 2 `v0.5.1`

# 1.0.0 (Stability)
Published: 2022-07-16T23:51:10Z

## Changes
_non-breaking:_
- Add configuration option for specifying the remote name used for pushing to GitHub if it should not be "origin"
- Find a GitHub Pull Request template file whether Harmony is run from the root of the `git` repo or a subfolder.

### Notes
ℹ️ This is the first technically stable release. I have not needed to create any breaking changes for a couple of versions and the app is quite useful in its current form, so it feels like time to commit to not breaking things for a while now.


You can build the source with Idris 2 `v0.5.1`

# 0.7.1 (Who Am I Again?)
Published: 2022-07-15T07:19:18Z

- Fix bug where color support and configuration was ignored for the `list` command which always output with ANSI formatting.
- Make `whoami` output prettier by adding a bit of color and formatting.


You can build the source with Idris 2 `v0.5.1`

# 0.7.0 (Who Am I?)
Published: 2022-07-13T16:58:03Z

Adds a `whoami` command that prints information about the currently configured and authenticated user.


You can build the source with Idris 2 `v0.5.1`

# 0.6.0 (`--checkout` option and `branch` command)
Published: 2022-07-13T00:57:25Z

- The `contribute` command now has a `--checkout` option that checks the branch out in addition to printing the GItHub URI for the Pull Request in need of a review.
- There is now a `branch` command that prints the GitHub URI for the currently checked out branch.
- The help menu colors have changed slightly to be more visible against nearly-black terminal backgrounds.
- The shell auto-completion handling has been slightly improved.


You can build the source with Idris 2 `v0.5.1`


# 0.5.0 (EDITOR support, pull request template support, NO_COLOR support)
Published: 2022-03-28T05:48:44Z

Adds support for the `NO_COLOR` and `EDITOR` environment variables. The latter of which is used when asking for Pull Request descriptions, which now also support GitHub PR templates (or at least the specific case where a `.github/PULL_REQUEST_TEMPLATE.md` file is present).


You can build the source with Idris 2 `v0.5.1`

# 0.4.0 (Config)
Published: 2021-10-08T04:24:36Z

- Adds a `config` command that allows getting and setting of some portion of the harmony configuration. So far, just whether or not to assign teams in addition to individuals when requesting reviews and whether or not to comment on pull requests when requesting reviews.
- Asks for additional configuration options when initially creating the configuration file.
- Adds more details to the output of the `graph` command.


You can build the source with Idris 2 v0.5.1

# 0.3.0 (Contribute)
Published: 2021-10-01T19:24:06Z

- Adds a `contribute` command that suggests a PR you could review to contribute to the org.
- Adds information on reviews you have given to the output of the `reflect` command.


You can build the source with Idris 2 v0.5.1

# 0.2.0 (Reflect)
Published: 2021-09-25T05:23:38Z

This release adds a `reflect` command that summarizes your PR history over the past 100 PRs.


You can build the source with Idris 2 v0.5.1

# 0.1.0 (Initial Release)
Published: 2021-09-13T00:25:53Z

The first release of Harmony.


You can build the source with Idris 2 v0.5.1
