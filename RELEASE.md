
## Release Checklist
- [ ] Update the docs/ files
- [ ] Generate README.md
- [ ] Generate man/harmony.1
- [ ] Bump versions in `package.json`, `README.md`, `src/AppVersion.idr`, and `harmony.ipkg` (`make version v=1.2.3`)
- [ ] Publish to NPM
- [ ] Create new GitHub Release
- [ ] Run `./scripts/generate-changelog.sh latest`
