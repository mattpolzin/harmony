#/bin/sh

pkgversion="$(cat harmony.ipkg | sed -n 's/version = \(.*\)/\1/p')"
idrversion="$(cat src/AppVersion.idr | sed -n 's/appVersion = "\(.*\)"/\1/p')"
npmversion="$(cat package.json | jq -r .version)"

if [ "$pkgversion" == "$npmversion" ] && [ "$pkgversion" == "$idrversion" ]; then
  exit 0
else
  echo "Idris package manifest version (${pkgversion}), in-source version (${idrversion}), and NPM package version (${npmversion}) do not agree!"
  exit 1
fi

