
const SimpleGit = require('simple-git')

const git_git = () =>
  SimpleGit()

const idris__git_unpromisify = (promise, onSuccess, onFailure) =>
  promise.then(r => onSuccess(r)(), e => onFailure(e)())

// trim a result (second argument) and pass it to the given callback (first argument).
const idris__git_trim = callback => value => callback(value.trim())

// current branch
// @Returns String
const git_current_branch = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('branch', '--show-current'),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_checkout_branch = (git, branch, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('checkout', `${branch}`),
    r => onSuccess(''),
    onFailure()
  )

// push the current branch, setting its upstream
// Executes callback with empty string on success.
const git_push_new_branch = (git, remoteName, branch, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('push', '--set-upstream', remoteName, `${branch}`),
    r => onSuccess(''),
    onFailure
  )

const git_push = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('push'),
    r => onSuccess(''),
    onFailure
  )

// remote URI
const git_remote_uri = (git, remoteName, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('remote', 'get-url', remoteName),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_list_remotes = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('remote'),
    idris__git_trim(onSuccess),
    onFailure
  )

// Get the remote branch that the current local branch is tracking.
// Executes callback with branch name or empty string when no branch
// is being tracked.
const git_remote_tracking_branch = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('symbolic-ref', '-q', 'HEAD')
      .then(headRef => 
        git.raw('for-each-ref', '--format', '%(upstream:short)', `${headRef.trim()}`),
        onFailure),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_uncommitted_changes = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('diff', '--name-only'),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_staged_changes = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('diff', '--staged', '--name-only'),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_unpushed_commits = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('log', '@{push}..'),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_user_email = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('config', '--get', 'user.email'),
    idris__git_trim(onSuccess),
    onFailure
  )

const git_root_dir = (git, onSuccess, onFailure) =>
  idris__git_unpromisify(
    git.raw('rev-parse', '--show-toplevel'),
    idris__git_trim(onSuccess),
    onFailure
  )

