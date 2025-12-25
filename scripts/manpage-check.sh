#/bin/sh

# check that the manpage was generated based on latest changes to its source
# files.
git stash || :
make man/harmony.1
git diff --exit-code
EXITC=$?
git stash pop || :
exit $EXITC

