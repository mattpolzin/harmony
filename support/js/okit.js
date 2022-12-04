
const { Octokit } = require('octokit')

const okit_octokit = authToken =>
  new Octokit({ auth: authToken })

const idris__okit_unpromisify = (promise, onSuccess, onFailure) =>
  promise.then(r => onSuccess(r)(), e => onFailure(e)())

const idris__okit_stringify_error = (fn) => (err) => {
  const url = err.response.url
  const msg = err.response.data.message
  const details =
    Array.isArray(err.response.data.errors)
    ? '\n - ' + err.response.data.errors.map(e => e.message).join('\n - ')
    : ''
  return fn('GitHub Error: ' + msg + ' (' + url + ')' + details)
}

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
    idris__okit_stringify_error(onFailure)
  )

// get repo labels
const digLabelNames = labelsJson =>
  labelsJson.map(l => l.name)

const okit_list_repo_labels = (octokit, org, repo, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.listLabelsForRepo({ owner: org, repo, per_page: 100 }),
    r => onSuccess(newline_delimited(digLabelNames(r.data))),
    idris__okit_stringify_error(onFailure)
)

// list teams
const digTeams = teamsJson =>
  teamsJson.map(t => t.slug)

// Executes callback with [String] (string array)
const okit_list_teams = (octokit, org, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.list({ org, per_page: 100 }),
    r => onSuccess(newline_delimited(digTeams(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// Executes callback with [String] (string array)
const okit_list_my_teams = (octokit, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.listForAuthenticatedUser({per_page: 100}),
    r => onSuccess(newline_delimited(digTeams(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// list PRs for branch
const digPr = pr => {
    return {
      pull_number: pr.number,
      author: pr.user.login,
      state: pr.state,
      created_at: pr.created_at,
      merged: pr.merged_at !== null,
      reviewers: pr.requested_reviewers.map(u => u.login),
      head_ref: pr.head.ref
    }
  }
const digPrs = prJson =>
  prJson.map(digPr)

// List PRs for a branch
// Executes callback with stringified JSON [{"pull_number": Int, "author": String}]
const okit_list_pull_requests_for_branch = (octokit, owner, repo, branch, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, head: `${owner}:${branch}`, state: 'open', per_page: 10 }),
    r => onSuccess(JSON.stringify(digPrs(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// Create PR
// Executes callback with stringified JSON {"pull_number": Int, "author": String}
const okit_create_pr = (octokit, owner, repo, head, base, title, body, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.create({ owner, repo, head, base, title, body }),
    r => onSuccess(JSON.stringify(digPr(r.data))),
    idris__okit_stringify_error(onFailure)
  )


// Create PR -OR- Issue Comment
// Executes callback with "" (empty string)
const okit_create_comment = (octokit, owner, repo, issue_number, body, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.createComment({ owner, repo, issue_number: Number(issue_number), body }),
    r => onSuccess(""),
    idris__okit_stringify_error(onFailure)
  )

// list PR reviewers
const digReviewers = prJson =>
  prJson.flatMap(pr => pr.requested_reviewers.map(u => u.login))

// Executes callback with [String]
const okit_list_reviewers = (octokit, owner, repo, state, per_page, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, state, per_page }),
    r => onSuccess(newline_delimited(digReviewers(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// list PRs
// Executes callback with [{ "pull_number": Int, "author": String, "state": String, "merged": Boolean, "reviewers": [String] }]
const okit_list_pull_requests = (octokit, owner, repo, state, per_page, page, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, state, per_page, page }),
    r => onSuccess(JSON.stringify(digPrs(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// add PR reviewers
// @param reviewers String A comma separated list of reviewer logins.
// @param teamReviewers String A comma separated list of team slugs.
// Executes callback with [String] (logins for all reviewers).
const okit_add_reviewers = (octokit, owner, repo, pull_number, reviewers, team_reviewers, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.requestReviewers({ owner, repo, pull_number: Number(pull_number), reviewers: from_comma_delimited(reviewers), team_reviewers: from_comma_delimited(team_reviewers) }),
    r => onSuccess(newline_delimited(digReviewers([r.data]))),
    idris__okit_stringify_error(onFailure)
  )

// list PR reviews
const digReviews = reviewsJson =>
  reviewsJson.map(review => {
    return {
      author: review.user.login,
      state: review.state,
      submitted_at: review.submitted_at
    }
  })

// Add PR labels
// Returns all labels currently applied to the PR.
const okit_add_labels = (octokit, owner, repo, pull_number, labels, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.addLabels({ owner, repo, issue_number: Number(pull_number), labels: from_comma_delimited(labels) }),
    r => onSuccess(newline_delimited(digLabelNames(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// Executes callback with [{author: String, state: String, submitted_at: String}]
const okit_list_pr_reviews = (octokit, owner, repo, pull_number, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.listReviews({ owner, repo, pull_number: Number(pull_number) }),
    r => onSuccess(JSON.stringify(digReviews(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// list team members
const digUserLogins = usersJson =>
  usersJson.map(u => u.login)

// Executes callback with [String]
const okit_list_team_members = (octokit, org, team_slug, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.listMembersInOrg({ org, team_slug }),
    r => onSuccess(newline_delimited(digUserLogins(r.data))),
    idris__okit_stringify_error(onFailure)
  )

// list org members
// Executes callback with [String]
const okit_list_org_members = (octokit, org, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.orgs.listMembers({ org, per_page: 100 }),
    r => onSuccess(newline_delimited(digUserLogins(r.data))),
    idris__okit_stringify_error(onFailure)
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
    idris__okit_stringify_error(onFailure)
  )

// get authed user details (self)
// Executes callback with { login: String, name: String }
const okit_get_self = (octokit, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.users.getAuthenticated(),
    r => onSuccess(JSON.stringify(digUser(r.data))),
    idris__okit_stringify_error(onFailure)
  )

