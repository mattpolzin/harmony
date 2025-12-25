## Installation
For any installation, you need to have a GitHub 
[Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).

Your Personal Access Token should have the following permissions:
- `repo` (Full control of private repositories)
- `read:org` (Read org and team membership, read org projects)
- `read:user`
- `user:email`
- `read:discussion`
- `read:enterprise` (Read enterprise profile data)

You can either add the PAT to your environment as the `GITHUB_PAT` (or
alternatively `GH_TOKEN`) variable (perhaps exporting it from your shell
resource file or profile) or you can store your PAT in Harmony's config file.
The first time you start Harmony, it will ask you to configure your PAT if you
don't want to use the Environment variable. You only need one of (a) the ENV var
and (b) the config property and the environment variable will take precedence if
you have both set.

### NPM
You can install Harmony via npm directly by running 
`npm install -g @mattpolzin/harmony`.

### GitHub Release
You can install any Harmony release by downloading the `harmony-npm.tar.gz` file
from the GitHub Release page, unzipping it, and running `npm install --global`.

### Nix Flake
You can add Harmony to your Flake inputs as follows:
```nix
  inputs = {
    ...
    harmony.url = "github:mattpolzin/harmony";
    harmony.inputs.nixpkgs.follows = "nixpkgs";
  };
```

Then, in your outputs, bring Harmony into a package install list as
`harmony.packages.<system>.harmony`.

Harmony builds are cached in Cachix so you can take advantage of those builds by
adding `https://gh-harmony.cachix.org` to the list of `substituters` and
`gh-harmony.cachix.org-1:KX5tTtEt3Y6an8pywe3Cy6jR9bUo+1Cl7hJmh+5eI4I=` to the
list of `trusted-public-keys`.

### From Source
The build script assumes a HEAD build of Idris 2 is installed on your system.
For an alternative, see the [Docker Build](#docker-build) instructions below.

Build Harmony from source with a call to `make`. Then install it globally with
`make install`.

### Docker Build
If you want to build Harmony without installing Idris 2 on your system, you can
build Harmony within a Docker container and then install the resulting
Javascript onto your system.

First, download the latest nightly Docker image:
```shell
docker pull mattpolzin2/idris-docker:nightly
```

Then, from a directory containing this Harmony git repository, build Harmony:
```shell
docker run --rm -v "$(pwd):/build" \
  mattpolzin2/idris-docker:nightly \
  bash -c "apt-get update && apt-get install -y git && cd /build && make"
```

At this point you are done with Docker. From the same directory, install Harmony
globally:
```shell
npm install --global
```

