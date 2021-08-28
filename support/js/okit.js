
const { Octokit } = require('octokit')

const okit_octokit = authToken =>
  new Octokit({ auth: authToken })

const idris__unpromisify = (promise, onSuccess, onFailure) =>
  promise.then(r => onSuccess(r)(), e => onFailure(e)())

const newline_delimited = array =>
  array.join("\n")

// list PR reviewers
const reviewers = prJson =>
  prJson.flatMap(pr => pr.requested_reviewers.map(u => u.login))

// @Returns [String]
const okit_list_reviewers = (octokit, owner, repo, state, onSuccess, onFailure) =>
  idris__unpromisify(
    octokit.rest.pulls.list({ owner, repo, state, per_page: 100 }),
    r => onSuccess(newline_delimited(reviewers(r.data))),
    onFailure
  )

// list team members
const team_members = teamJson =>
  teamJson.map(u => u.login)

// @Returns [String]
const okit_list_team_members = (octokit, org, team_slug, onSuccess, onFailure) =>
  idris__unpromisify(
    octokit.rest.teams.listMembersInOrg({ org, team_slug }),
    r => onSuccess(newline_delimited(team_members(r.data))),
    onFailure
  )
