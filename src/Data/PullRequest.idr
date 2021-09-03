module Data.PullRequest

%default total

public export
record PullRequest where
  constructor MkPullRequest
  ||| The pull request's "number" (as seen in URIs referring to the PR).
  number : Integer
  ||| The `login` of the author of the pull request.
  author : String

%name PullRequest pr, pr1, pr2

export
Show PullRequest where
  show (MkPullRequest number author) = "(\{show number}, \{show author})"

