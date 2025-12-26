#/bin/sh

# Skip the installation section for the manpage. Presumably this thing has been
# installed by the time the manpage is most useful.
FILES="$(find ./docs -name '*.md' | sort | grep -v '_3.*Installation.md')"

OUTPUT_FILE="${1:-man/harmony.1}"

# Remove image tags (not supported in manpages) and generate
# manpage via pandoc
cat ${FILES} | \
  sed 's#!\[.*\](.*)##' | \
  pandoc \
  --standalone \
  --to man \
  -o "${OUTPUT_FILE}"
