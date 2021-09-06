
const SimpleGit = require('simple-git')

const git_git = () =>
  SimpleGit()

const idris__git_unpromisify = (promise, onSuccess, onFailure) =>
  promise.then(r => onSuccess(r)(), e => onFailure(e)())

// current branch
// @Returns String
const git_current_branch = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('branch', '--show-current'),
    r => onSuccess(r.trim()),
    onFailure
  )

// remote URI
const git_remote_uri = (git, remoteName, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('remote', 'get-url', remoteName),
    r => onSuccess(r.trim()),
    onFailure
  )

