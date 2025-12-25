#/bin/sh

# check that the manpage was generated based on latest changes to its source
# files.
sh ./scripts/generate-manpage.sh man/harmony.1.generated
diff man/harmony.1 man/harmony.1.generated
EXITC=$?
rm -f ./man/harmony.1.generated
exit $EXITC

