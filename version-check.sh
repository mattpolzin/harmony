#/bin/sh

pkgversion="$(cat harmony.ipkg | sed -n 's/version = \(.*\)/\1/p')"
idrversion="$(cat src/AppVersion.idr | sed -n 's/appVersion = "\(.*\)"/\1/p')"
npmversion="$(cat package.json | jq -r .version)"
nixversion="$(cat default.nix | sed -n 's/.*version = "\(.*\)";/\1/p')"

if [ "$pkgversion" == "$npmversion" ] && [ "$pkgversion" == "$idrversion" ] && [ "$pkgversion" == "$nixversion" ]; then
  exit 0
else
  echo "Idris package manifest version (${pkgversion}), in-source version (${idrversion}), NPM package version (${npmversion}), and Nix version (${nixversion}) do not agree!"
  exit 1
fi

