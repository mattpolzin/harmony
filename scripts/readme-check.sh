#!/bin/sh

# check that the README was generated based on latest changes to its source
# files.
sh ./scripts/generate-readme.sh README.md.generated
diff README.md README.md.generated
EXITC=$?
rm -f ./README.md.generated
if [ $"EXITC" = 0 ]; then
  echo 'README is up to date.'
fi
exit $EXITC

