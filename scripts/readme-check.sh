#/bin/sh

# check that the README was generated based on latest changes to its source
# files.
git stash || :
make README.md
git diff --exit-code
EXITC=$?
git stash pop || :
exit $EXITC

