
const { Octokit } = require('octokit')
const { GraphqlResponseError } = require('@octokit/graphql')

const okit_octokit = authToken =>
  new Octokit({ auth: authToken })

// Note that every Octokit FFI function uses idris__okit_stringify_error
// which results in onFailure being called with a JSON string of the following structure:
// { "status": <http status code>, "error": <error string> }
const idris__okit_unpromisify = (promise, onSuccess, onFailure) =>
  promise.then(r => onSuccess(r)(), e => idris__okit_stringify_error(onFailure)(e)())

const idris__okit_stringify_error = (fn) => (err) => {
  if (err instanceof GraphqlResponseError) {
    const status = err.headers.status || 422
    const query = err.request.query
    const msg = err.message
    const json = { status,  error: 'Github Error: ' + msg + ' (' + query + ')' }
    return fn(JSON.stringify(json))
  } else {
    const status = err.response.status;
    const url = err.response.url
    const msg = err.response.data.message
    const details =
      Array.isArray(err.response.data.errors)
      ? '\n - ' + err.response.data.errors.map(e => e.message).join('\n - ')
      : ''
    const json = { status, error: 'GitHub Error: ' + msg + ' (' + url + ')' + details }
    return fn(JSON.stringify(json))
  }
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

const okit_get_repo_default_branch = (octokit, owner, repo, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.repos.get({ owner, repo }),
    r => onSuccess(digDefaultBranch(r.data)),
    onFailure
  )

// get repo labels
const digLabelNames = labelsJson =>
  labelsJson.map(l => l.name)

const okit_list_repo_labels = (octokit, owner, repo, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.listLabelsForRepo({ owner, repo, per_page: 100 }),
    r => onSuccess(newline_delimited(digLabelNames(r.data))),
    onFailure
)

// list teams
const digTeams = teamsJson =>
  teamsJson.map(t => t.slug)

// Executes callback with [String] (string array)
const okit_list_teams = (octokit, org, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.list({ org, per_page: 100 }),
    r => onSuccess(newline_delimited(digTeams(r.data))),
    onFailure
  )

// Executes callback with [String] (string array)
const okit_list_my_teams = (octokit, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.teams.listForAuthenticatedUser({per_page: 100}),
    r => onSuccess(newline_delimited(digTeams(r.data))),
    onFailure
  )

// list PRs for branch
const digPr = pr => {
    return {
      pull_number: pr.number,
      author: pr.user.login,
      state: pr.state,
      created_at: pr.created_at,
      draft: pr.draft,
      merged: pr.merged_at !== null,
      reviewers: pr.requested_reviewers.map(u => u.login),
      head_ref: pr.head.ref,
      base_ref: pr.base.ref,
      title: pr.title
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
    onFailure
  )

// Create PR
// Executes callback with stringified JSON {"pull_number": Int, "author": String}
const okit_create_pr = (octokit, owner, repo, head, base, title, body, isDraft, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.create({ owner, repo, head, base, title, body, draft: Boolean(isDraft) }),
    r => onSuccess(JSON.stringify(digPr(r.data))),
    onFailure
  )

// Get GraphQL PR Data
const digGraphQlPr = pr => {
    return {
      graphql_id: pr.id,
      pull_number: pr.number,
      author: pr.author.login,
      state: pr.state.toLowerCase(),
      created_at: pr.createdAt,
      draft: pr.isDraft,
      merged: pr.merged,
      reviewers: pr.reviewRequests.nodes.map(rr => rr.requestedReviewer.login),
      head_ref: pr.headRefName,
      base_ref: pr.baseRefName,
      title: pr.title
    }
  }

const graphql_pr_selections = `
            id
            number
            author { ... on Actor { login } }
            state
            createdAt
            isDraft
            merged
            reviewRequests(first: 100) { nodes { requestedReviewer { ... on Actor { login } ... on Team { slug } } } }
            headRefName
            baseRefName
            title
`

const okit_get_pr_graphql_id = (octokit, owner, repo, pull_number, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.graphql({
      query: `query getPr($owner: String!, $repo: String!, $pull_number: Int!) {
        repository(owner: $owner, name: $repo) {
          pullRequest(number: $pull_number) {
            ${graphql_pr_selections}
          }
        }
      }`,
      owner,
      repo,
      pull_number: Number(pull_number)
    }),
    r => onSuccess(digGraphQlPr(r.repository.pullRequest).graphql_id),
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

// list PRs
// Executes callback with [{ "pull_number": Int, "author": String, "state": String, "merged": Boolean, "reviewers": [String], "title": String }]
const okit_list_pull_requests = (octokit, owner, repo, state, per_page, page, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.list({ owner, repo, state, per_page, page }),
    r => onSuccess(JSON.stringify(digPrs(r.data))),
    onFailure
  )

// add PR reviewers
// @param reviewers String A comma separated list of reviewer logins.
// @param teamReviewers String A comma separated list of team slugs.
// Executes callback with [String] (array of logins for all reviewers).
const okit_add_reviewers = (octokit, owner, repo, pull_number, reviewers, team_reviewers, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.requestReviewers({ owner, repo, pull_number: Number(pull_number), reviewers: from_comma_delimited(reviewers), team_reviewers: from_comma_delimited(team_reviewers) }),
    r => onSuccess(newline_delimited(digReviewers([r.data]))),
    onFailure
  )

// set PR to draft
// @param opaque_pr_graphql_id String The GraphQL Id for the Pull Request
// Executes callback with [{ "graphql_id": String, "pull_number": Int, "author": String, "state": String, "merged": Boolean, "reviewers": [String], "title": String }]
const okit_mark_pr_draft = (octokit, opaque_pr_graphql_id, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.graphql({
      query: `mutation convertToDraft($opaque_pr_graphql_id: ID!) {
        convertPullRequestToDraft(input: {pullRequestId: $opaque_pr_graphql_id}) {
          pullRequest  {
            ${graphql_pr_selections}
          }
        }
      }`,
      opaque_pr_graphql_id
    }),
    r => onSuccess(JSON.stringify(digGraphQlPr(r.convertPullRequestToDraft.pullRequest))),
    onFailure
  )

// set PR to ready for review
// @param opaque_pr_graphql_id String The GraphQL Id for the Pull Request
// Executes callback with [{ "graphql_id": String, "pull_number": Int, "author": String, "state": String, "merged": Boolean, "reviewers": [String], "title": String }]
const okit_mark_pr_ready = (octokit, opaque_pr_graphql_id, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.graphql({
      query: `mutation convertToReady($opaque_pr_graphql_id: ID!) {
        markPullRequestReadyForReview(input: {pullRequestId: $opaque_pr_graphql_id}) {
          pullRequest  {
            ${graphql_pr_selections}
          }
        }
      }`,
      opaque_pr_graphql_id
    }),
    r => onSuccess(JSON.stringify(digGraphQlPr(r.convertPullRequestToReady.pullRequest))),
    onFailure
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
    onFailure
  )

// Executes callback with [{author: String, state: String, submitted_at: String}]
const okit_list_pr_reviews = (octokit, owner, repo, pull_number, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.pulls.listReviews({ owner, repo, pull_number: Number(pull_number) }),
    r => onSuccess(JSON.stringify(digReviews(r.data))),
    onFailure
  )

// list PRs for branch
const digIssue = issue => {
    return {
      issue_number: issue.number,
      author: issue.user.login,
      created_at: issue.created_at,
      title: issue.title,
      body: issue.body,
      assignee: issue.assignee ? issue.assignee.login : null
    }
  }
const digIssues = issueJson =>
  issueJson.map(digIssue)

// Create Issue
// Executes callback with stringified JSON {"issue_number": Int, "author": String, "created_at": Date, "title": String, "body": String?, "assignee": String?}
const okit_create_issue = (octokit, owner, repo, title, body, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.create({ owner, repo, title, body }),
    r => onSuccess(JSON.stringify(digIssue(r.data))),
    onFailure
  )

// Get a single Issue
// Executes callback with stringified JSON {"issue_number": Int, "author": String, "created_at": Date, "title": String, "body": String?, "assignee": String?}
const okit_get_issue = (octokit, owner, repo, issue_number, onSuccess, onFailure) =>
  idris__okit_unpromisify(
    octokit.rest.issues.get({ owner, repo, issue_number }),
    r => onSuccess(JSON.stringify(digIssue(r.data))),
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
    octokit.rest.users.getAuthenticated(),
    r => onSuccess(JSON.stringify(digUser(r.data))),
    onFailure
  )

