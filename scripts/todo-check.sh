#/bin/sh

todos=$(find . \( -name '*.idr' -o -name '*.js' \) ! -path '*build*' ! -path '*node_modules*' | xargs grep -U1 'TODO')

pkgversion="$(cat harmony.ipkg | sed -n 's/version = \(.*\)/\1/p')"

if [[ "$todos" != '' ]]; then
  echo "TODOs found:"
  echo "$todos"

  criticals="$(echo "$todos" | sed -n "s/^.*TODO $pkgversion/TODO $pkgversion/p")"

  if [[ "$criticals" != '' ]]; then
    echo ""
    echo "Cannot proceeed with critical TODOs for the current version number:"
    echo "$criticals"
    exit 1
  fi
fi

