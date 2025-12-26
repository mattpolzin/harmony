## Dependencies
### Runtime
Running Harmony requires NodeJS 18+ (and a local installation of `git`) or
alternatively Nix with flakes enabled.

If you'd like to try Harmony out without even "installing" it and you have Nix
installed with flakes enabled, you can run it as 
`nix run github:mattpolzin/harmony`.

### Build time
Building the latest commits of Harmony requires a HEAD build of the Idris 2
compiler. Each release page also indicates the version of Idris 2 that
particular release will build against.

Alternatively, you can build Harmony with Docker (see 
[Docker Build](#docker-build)).

### Testing
Tests can be run with `make test`. You'll need a few common tools in your `PATH`
to run all the tests:
  - `realpath`
  - `sed`
  - `xargs`

