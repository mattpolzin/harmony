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
The build script assumes `pack` with a recent version of the package collection
is installed on your system.

Build Harmony from source with a call to `make`. Then install it globally with
`make install`.

