#/bin/sh

idrversion="$(cat harmony.ipkg | sed -n 's/version = \(.*\)/\1/p')"
npmversion="$(cat package.json | jq -r .version)"

if [ "$idrversion" == "$npmversion" ]; then
  exit 0
else
  echo "Idris manifest version (${idrversion}) and NPM package version (${npmversion}) do not agree!"
  exit 1
fi

