
# Harmony
Harmony is a small tool that helps teams keep GitHub reviews running smoothly. It takes the work out of picking someone from a pool of developers to review a new PR. Harmony does this by heuristically determining who on a particular GitHub Team has the least current/recent review workload.

## Dependencies
Building Harmony requires a HEAD build of the Idris 2 compiler but running it only requires NodeJS 12+ (and a local installation of `git`).

## Installation
Build Harmony from source with a call to `make`. Then install it globally with `make install`.

You need to add a GitHub [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to your environment as the `GITHUB_PAT` variable. It's easiest to `export` that variable from your shell resource file or profile.

### Bash completion
Set up back completion by adding the following to your Bash resource file or profile:
```shell
eval "$(harmony --bash-completion-script)"
```

## Usage
The first time you start Harmony in any particular folder, you will be asked to provide some information about the GitHub repository you are working with. This information is stored in a file named `harmony.json` in the current working directory.

Note that the GitHub organization and repository are both slugs, not names. These are the values you find in a GitHub URL pointing to your repository. If you are working with a personal repository, "org" here is synonymous with "username".
```shell
$ harmony
Creating a new configuration (storing in harmony.json)...
What GitHub org would you like to use harmony for?
some-org
What repository would you like to use harmony for?
some-repo
What is the base/main branch (e.g. 'main')?
main
```

Once configured, Harmony supports two commands: `pr` and `assign`.

Running `harmony pr` with a branch checked out will reach out to GitHub to determine if there is an open PR for that branch. If there is a PR, Harmony will print a URI that can be used to view the PR. IF there is not a PR, Harmony will help you create one.

Running `harmony assign <team>` will help you create a PR if one does not exist yet and then it will pick someone to review the PR and assign both that user and the team they belong to as reviewers of the PR.

