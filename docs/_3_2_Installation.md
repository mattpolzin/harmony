## Installation
You can install Harmony in several different ways, though by far the easiest is
to install it via the Node Package Manager (NPM).

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

