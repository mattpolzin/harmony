#!/bin/sh
MODE=all
if [ "$1" = 'latest' ]; then
  MODE=latest
else
  if [ "$1" != 'all' ] && [ "$(cat CHANGELOG.md)" != '' ]; then
    MODE=latest
  fi
fi

GIT_TAGS="$(git tag -l --sort -committerdate)"

if [ "${MODE}" = 'latest' ]; then
  GIT_TAGS="$(echo "${GIT_TAGS}" | head -1)"
fi

REMOVE_CARRIAGE_RETURNS='s/\\r//g'

REMOVE_INSTALL_INSTRUCTIONS='/^-----/,+1d'

NOTES=$(echo "${GIT_TAGS}" | \
  xargs -n1 gh release view --json body,publishedAt,name,tagName | \
  sed "${REMOVE_CARRIAGE_RETURNS}" | \
  jq -r '["# " + .tagName + " (" + .name + ")","Published: " + .publishedAt,"",.body,""] | join("\n")' | \
  sed "${REMOVE_INSTALL_INSTRUCTIONS}")

REST="$(cat CHANGELOG.md)"
echo "${NOTES}" > CHANGELOG.md

if [ "${MODE}" = 'latest' ]; then
  echo "" >> CHANGELOG.md
  echo "${REST}" >> CHANGELOG.md
fi
