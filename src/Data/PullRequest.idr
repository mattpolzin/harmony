module Data.PullRequest

import Data.Config
import Language.JSON
import Language.JSON.Accessors
import Data.Vect
import Data.Either
import Data.List

%default total

public export
data PRState = Open | Closed

export
Show PRState where
  show Open   = "open"
  show Closed = "closed"

export
Eq PRState where
  Open   == Open   = True
  Closed == Closed = True
  _ == _ = False

public export
record PullRequest where
  constructor MkPullRequest
  ||| The pull request's "number" (as seen in URIs referring to the PR).
  number : Integer
  ||| The `login` of the author of the pull request.
  author : String
  ||| Open or Closed status.
  state  : PRState
  ||| A List of all reviewers requested on the PR.
  reviewers : List String

%name PullRequest pr, pr1, pr2

export
Show PullRequest where
  show (MkPullRequest number author state _) = "\{show number}: \{show author} (\{show state})"

export
(.webURI) : Config => PullRequest -> String
pr.webURI @{config} = "https://github.com/\{config.org}/\{config.repo}/pull/\{show pr.number}"

parsePR : JSON -> Either String PullRequest
parsePR json =
 do pr <- object json
    [pullNumber, authorLogin, stateStr, reviewerList] <- lookupAll ["pull_number", "author", "state", "reviewers"] pr
    number <- integer pullNumber
    author <- string authorLogin
    state  <- ?parseState <$> string stateStr
    reviewers <- array string reviewerList
    pure $ MkPullRequest {
        number
      , author
      , state
      , reviewers
      }

||| Parse a single user from a JSON String
export
parsePullRequestString : String -> Either String PullRequest
parsePullRequestString =
  (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parsePR

||| Parse a list of users from a JSON String
export
parsePullRequestsString : String -> Either String (List PullRequest)
parsePullRequestsString =
  (maybeToEither "Failed to parse JSON" . JSON.parse) >=> array parsePR

