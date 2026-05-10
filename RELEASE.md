
## Notes
Bumping the version will rebuild the manpage and README as well as swapping out all references to the current version number in various files.

## Release Checklist
- [ ] Update the docs/ files
- [ ] Bump versions in `package.json`, `README.md`, `src/AppVersion.idr`, and `harmony.ipkg` (`make version v=1.2.3`)
- [ ] Publish to NPM
- [ ] Create new GitHub Release
- [ ] Run `./scripts/generate-changelog.sh latest`
