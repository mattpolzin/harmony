#!/bin/sh

# Skip the manpage header and author sections and any existing index.
FILES="$(find ./docs -name '*.md' | sort | grep -v '_0.*MANPAGE_HEADER.md' | grep -v '_5.*AUTHOR.md' | grep -v '_1_9_INDEX.md')"

OUTPUT_FILE="${1:-README.md}"

# Generate without index
cat ${FILES} > "${OUTPUT_FILE}"

# Generate index
INDEX_FILE_NAME='./docs/_1_9_INDEX.md'
INDEX="$(cat "${OUTPUT_FILE}" | sed -nE 's/^##? ([A-Za-z]+)$/- [\1](#\1)/p')"

echo "# INDEX" > "${INDEX_FILE_NAME}"
echo "" >> "${INDEX_FILE_NAME}"
echo "${INDEX}" >> "${INDEX_FILE_NAME}"
echo "" >> "${INDEX_FILE_NAME}"

FILES="$(echo "${FILES}" | cat - <(echo "${INDEX_FILE_NAME}") | sort)"

# Regenerate with index
cat ${FILES} > "${OUTPUT_FILE}"
