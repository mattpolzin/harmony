
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

// get repo default branch
const digDefaultBranch = repoJson =>
  repoJson.default_branch

const okit_get_repo_default_branch = (octokit, org, repo, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.repos.get({ owner: org, repo }),
    r => onSuccess(digDefaultBranch(r.data)),
    onFailure
  )

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
const digPr = pr => {
    return {
      pull_number: pr.number,
      author: pr.user.login,
      state: pr.state,
      reviewers: pr.requested_reviewers.map(u => u.login)
    }
  }
const digPrs = prJson =>
  prJson.map(digPr)

// Executes callback with stringified JSON [{"pull_number": Int, "author": String}]
const okit_list_prs = (octokit, owner, repo, branch, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, head: `${owner}:${branch}`, state: 'open', per_page: 10 }),
    r => onSuccess(JSON.stringify(digPrs(r.data))),
    onFailure
  )

// Create PR
// Executes callback with stringified JSON {"pull_number": Int, "author": String}
const okit_create_pr = (octokit, owner, repo, head, base, title, body, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.create({ owner, repo, head, base, title, body }),
    r => onSuccess(JSON.stringify(digPr(r.data))),
    onFailure
  )


// Create PR -OR- Issue Comment
// Executes callback with "" (empty string)
const okit_create_comment = (octokit, owner, repo, issue_number, body, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.createComment({ owner, repo, issue_number: Number(issue_number), body }),
    r => onSuccess(""),
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

// list PR reviewers
// Executes callback with [{ "pull_number": Int, "author": String, "state": String, "reviewers": [String] }]
const okit_list_pull_requests = (octokit, owner, repo, state, per_page, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, state, per_page }),
    r => onSuccess(JSON.stringify(digPrs(r.data))),
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
const digUserLogins = usersJson =>
  usersJson.map(u => u.login)

// Executes callback with [String]
const okit_list_team_members = (octokit, org, team_slug, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.listMembersInOrg({ org, team_slug }),
    r => onSuccess(newline_delimited(digUserLogins(r.data))),
    onFailure
  )

// list org members
// Executes callback with [String]
const okit_list_org_members = (octokit, org, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.orgs.listMembers({ org, per_page: 100 }),
    r => onSuccess(newline_delimited(digUserLogins(r.data))),
    onFailure
  )

// get user details
const digUser = userJson => {
  return { login: userJson.login, name: userJson.name }
}

// Executes callback with { login: String, name: String }
const okit_get_user = (octokit, username, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.users.getByUsername({ username }),
    r => onSuccess(JSON.stringify(digUser(r.data))),
    onFailure
  )

// get authed user details (self)
// Executes callback with { login: String, name: String }
const okit_get_self = (octokit, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.users.getByUsername(),
    r => onSuccess(JSON.stringify(digUser(r.data))),
    onFailure
  )

