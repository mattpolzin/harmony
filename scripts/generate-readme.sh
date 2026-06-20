#!/bin/sh

INDEX_SECTION_IDX=1_9
INDEX_SECTION_NAME="_${INDEX_SECTION_IDX}_INDEX"

# Skip the manpage header and author sections and any existing index.
FILES="$(find ./docs -name '*.md' | sort | grep -v '_0.*MANPAGE_HEADER.md' | grep -v '_5.*AUTHOR.md' | grep -v "${INDEX_SECTION_NAME}.md")"

OUTPUT_FILE="${1:-README.md}"

# Generate without index
cat ${FILES} > "${OUTPUT_FILE}"

# Generate index
INDEX_FILE_NAME="./docs/${INDEX_SECTION_NAME}.md"
HEADINGS="$(cat "${OUTPUT_FILE}" | sed -nE 's/^##? ([A-Za-z ]+)$/\1/p' | sed 's/ /#/g')"

INDEX="$(
  for HEADING in ${HEADINGS}; do
    LINK="$(echo "${HEADING}" | sed 's/#/-/g' | tr '[:upper:]' '[:lower:]')"
    NAME="$(echo "${HEADING}" | sed 's/#/ /g')"
    echo "- [${NAME}](#${LINK})"
  done
)"

echo "# INDEX" > "${INDEX_FILE_NAME}"
echo "" >> "${INDEX_FILE_NAME}"
echo "${INDEX}" >> "${INDEX_FILE_NAME}"
echo "" >> "${INDEX_FILE_NAME}"

NEW_FILES="$(echo $FILES "${INDEX_FILE_NAME}" | xargs -n1 echo | sort)"

# Regenerate with index
cat ${NEW_FILES} > "${OUTPUT_FILE}"
