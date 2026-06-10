#!/bin/sh

# Skip the manpage header and author sections for the README.
FILES="$(find ./docs -name '*.md' | sort | grep -v '_0.*MANPAGE_HEADER.md' | grep -v '_5.*AUTHOR.md')"

OUTPUT_FILE="${1:-README.md}"

cat ${FILES} > "${OUTPUT_FILE}"
