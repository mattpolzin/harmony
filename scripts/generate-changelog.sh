#!/bin/sh

git tag -l --sort -committerdate | \
  xargs -n1 gh release view --json body,publishedAt,name,tagName | \
  sed 's/\\r//g' | \
  jq -r '["# " + .tagName + " (" + .name + ")","Published: " + .publishedAt,"",.body,""] | join("\n")' \
  > CHANGELOG.md
