#!/bin/sh

INDEX_SECTION_IDX=1_9
INDEX_SECTION_NAME="_${INDEX_SECTION_IDX}_INDEX"

# Skip the installation section for the manpage. Presumably this thing has been
# installed by the time the manpage is most useful.
FILES="$(find ./docs -name '*.md' | sort | grep -v "${INDEX_SECTION_NAME}.md" | grep -v '_3.*Installation.md' | grep -v '_3_1.*Buildtime.md' | grep -v '_3_1.*Testing.md')"

OUTPUT_FILE="${1:-man/harmony.1}"

# Remove image tags (not supported in manpages) and generate
# manpage via pandoc
cat ${FILES} | \
  sed 's#!\[.*\](.*)##' | \
  pandoc \
  --standalone \
  --to man \
  -o "${OUTPUT_FILE}"
