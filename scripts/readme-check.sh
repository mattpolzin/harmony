#/bin/sh

# check that the README was generated based on latest changes to its source
# files.
make README.md
git diff --exit-code

