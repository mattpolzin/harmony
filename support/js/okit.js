
const { Octokit } = require('octokit')

const okit_octokit = authToken =>
  new Octokit({ auth: authToken })

const idris__okit_unpromisify = (promise, onSuccess, onFailure) =>
  promise.then(r => onSuccess(r)(), e => onFailure(e)())

const newline_delimited = array =>
  array.join("\n")

const from_comma_delimited = str => {
  if (str === '') { return [] }
  return str.split(',')
}

// list teams
const digTeams = teamsJson =>
  teamsJson.map(t => t.slug)

// Executes callback with [String]
const okit_list_teams = (octokit, org, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.list({ org, per_page: 100 }),
    r => onSuccess(newline_delimited(digTeams(r.data))),
    onFailure
  )

// list PRs for branch
const digPrs = prJson =>
  prJson.map(pr => { return { pull_number: pr.number, author: pr.user.login } })

// Executes callback with stringified JSON {"pull_number": Int, "author": String}
const okit_list_pr_numbers = (octokit, owner, repo, branch, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, head: `${owner}:${branch}`, state: 'open', per_page: 10 }),
    r => onSuccess(JSON.stringify(digPrs(r.data))),
    onFailure
  )

// list PR reviewers
const digReviewers = prJson =>
  prJson.flatMap(pr => pr.requested_reviewers.map(u => u.login))

// Executes callback with [String]
const okit_list_reviewers = (octokit, owner, repo, state, per_page, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, state, per_page }),
    r => onSuccess(newline_delimited(digReviewers(r.data))),
    onFailure
  )

// add PR reviewers
// @param reviewers String A comma separated list of reviewer logins.
// @param teamReviewers String A comma separated list of team slugs.
// Executes callback with [String] (logins for all reviewers).
const okit_add_reviewers = (octokit, owner, repo, pull_number, reviewers, team_reviewers, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.requestReviewers({ owner, repo, pull_number: Number(pull_number), reviewers: from_comma_delimited(reviewers), team_reviewers: from_comma_delimited(team_reviewers) }),
    r => onSuccess(newline_delimited(digReviewers([r.data]))),
    onFailure
  )

// list team members
const digTeamMembers = teamJson =>
  teamJson.map(u => u.login)

// Executes callback with [String]
const okit_list_team_members = (octokit, org, team_slug, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.listMembersInOrg({ org, team_slug }),
    r => onSuccess(newline_delimited(digTeamMembers(r.data))),
    onFailure
  )

