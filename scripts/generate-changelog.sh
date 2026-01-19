#!/bin/sh
MODE=all
if [ "$1" = 'latest' ]; then
  MODE=latest
fi

GIT_TAGS="$(git tag -l --sort -committerdate)"

if [ "${MODE}" = 'latest' ]; then
  GIT_TAGS="$(echo "${GIT_TAGS}" | head -1)"
fi

NOTES=$(echo "${GIT_TAGS}" | \
  xargs -n1 gh release view --json body,publishedAt,name,tagName | \
  sed 's/\\r//g' | \
  jq -r '["# " + .tagName + " (" + .name + ")","Published: " + .publishedAt,"",.body,""] | join("\n")')

REST="$(cat CHANGELOG.md)"
echo "${NOTES}\n" > CHANGELOG.md

if [ "${MODE}" = 'latest' ]; then
  echo "" >> CHANGELOG.md
  echo "${REST}" >> CHANGELOG.md
fi
