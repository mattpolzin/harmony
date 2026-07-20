
## Notes
Bumping the version will rebuild the manpage and README as well as swapping out all references to the current version number in various files.

See the `release-addendum.md` file for text that should be appended to all release notes.

## Release Checklist
- [ ] Update the docs/ files
- [ ] Bump versions in `package.json`, `README.md`, `src/AppVersion.idr`, and `harmony.ipkg` (`make version v=1.2.3`)
- [ ] Publish to NPM
- [ ] Create new GitHub Release
- [ ] Run `make CHANGELOG.md`
