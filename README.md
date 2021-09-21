
# Harmony
Harmony is a small tool that helps teams keep GitHub reviews running smoothly. It takes the work out of picking someone from a pool of developers to review a new PR. Harmony does this by heuristically determining who on a particular GitHub Team has the least current/recent review workload.

## Dependencies
### Runtime
Running Harmony only requires NodeJS 12+ (and a local installation of `git`).
### Building
Building the latest commits of Harmony requires a HEAD build of the Idris 2 compiler. Each release page also indicates the version of Idris 2 that particular release will build against.

Alternatively, you can build Harmony with Docker (see [Docker Build](#docker-build)).

## Installation
### Release
You can install any Harmony release by downloading the `harmony-npm.tar.gz` file from the GitHub Release page, unzipping it, and running `npm install --global`.

### From Source
The normal installation assumes a HEAD build of Idris 2 is installed. For an alternative, see the [Docker Build](#docker-build) instructions below.

Build Harmony from source with a call to `make`. Then install it globally with `make install`.

You need to add a GitHub [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to your environment as the `GITHUB_PAT` variable. It's easiest to `export` that variable from your shell resource file or profile.

#### Docker Build
If you want to use Harmony without installing Idris 2 on your system, you can build Harmony within a Docker container and then install the resulting Javascript onto your system.

First, download the latest nightly Docker image:
```shell
docker pull mattpolzin2/idris-docker:nightly
```

Then, from a directory containing this Harmony git repository, build Harmony:
```shell
docker run --rm -v "$(pwd):/build" mattpolzin2/idris-docker:nightly bash -c "cd /build && make"
```

At this point you are done with Docker. From the same directory, install Harmony globally:
```shell
npm install --global
```

### Bash completion
Set up Bash completion by adding the following to your Bash resource file or profile:
```shell
eval "$(harmony --bash-completion-script)"
```

You can set up tab completion in Zsh, too:
```shell
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(harmony --bash-completion-script)"
```

## Usage
The first time you start Harmony in any particular folder, you will be asked to provide some information about the GitHub repository you are working with. This information is stored in a file named `harmony.json` in the current working directory.

Note that the GitHub organization and repository are both slugs, not names. These are the values you find in a GitHub URL pointing to your repository. Harmony does not work with personal repositories because they do not have teams or members.
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

Once configured, Harmony supports the following commands: `sync`, `pr`, `list`, `graph`, and `assign`.

### Sync
Running `harmony sync` will sync the locally configured team slugs and user logins that are used by auto-completion for Harmony. This sync is also performed automatically the first time you run Harmony after more than a day without the configuration being synced.

### PR
Running `harmony pr` with a branch checked out will reach out to GitHub to determine if there is an open PR for that branch. If there is a PR, Harmony will print a URI that can be used to view the PR. IF there is not a PR, Harmony will help you create one.

### List
Running `harmony list <team>` will list the members of the given GitHub Team.

### Graph
Running `harmony graph <team>` will graph the relative review workload of each of the members of the given GitHub Team.

### Assign
Running `harmony assign {<team> | +<user>} [...]` will help you create a PR if one does not exist yet and then it will pick someone to review the PR (from one of the listed teams) and assign both that user and the teams you listed as reviewers of the PR.

You can also require that specific additional users (on top of the one Harmony will pick for you) are assigned to the PR. You do this by specifying those users' logins prefixed with '+' as arguments to Harmony.

#### Examples
Assign the most available reviewer from the "developers" GitHub Team:
```shell
harmony assign developers
```

Assign the most available reviewer from either the "frontend" or "backend" GitHub Team:
```shell
harmony assign frontend backend
```

Assign the most available reviewer from the "web" team and additionally assign the users with logins "carl001" and "emmaham":
```shell
harmony assign web +carl001 +emmaham
```

