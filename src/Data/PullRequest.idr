module Data.PullRequest

import Data.Config
import Data.Date
import Data.Either
import Data.List
import Data.Vect
import Language.JSON
import Language.JSON.Accessors

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
  number    : Integer
  ||| When the PR was created.
  createdAt : Date
  ||| The `login` of the author of the pull request.
  author    : String
  ||| Open or Closed status.
  state     : PRState
  ||| A List of all reviewers requested on the PR.
  reviewers : List String

%name PullRequest pr, pr1, pr2

export
Show PullRequest where
  show (MkPullRequest number _ author state _) = "\{show number}: \{show author} (\{show state})"

export
(.webURI) : Config => PullRequest -> String
pr.webURI @{config} = "https://github.com/\{config.org}/\{config.repo}/pull/\{show pr.number}"

export
isAuthor : String -> PullRequest -> Bool
isAuthor login = (== login) . author

export
isRequestedReviewer : String -> PullRequest -> Bool
isRequestedReviewer login = any (== login) . reviewers

parseState : String -> Either String PRState
parseState "open"   = Right Open
parseState "closed" = Right Closed
parseState str      = Left "Failed to parse a Pull Request State (open/closed). Found \{str}."

parseDateTime : String -> Either String Date
parseDateTime = maybeToEither "Failed to parse Date" . parseDateTimeString

parsePR : JSON -> Either String PullRequest
parsePR json =
 do pr <- object json
    [pullNumber, authorLogin, stateStr, createdAtStr, reviewerList] <- lookupAll ["pull_number", "author", "state", "created_at", "reviewers"] pr
    number    <- integer pullNumber
    author    <- string authorLogin
    state     <- parseState    =<< string stateStr
    createdAt <- parseDateTime =<< string createdAtStr
    reviewers <- array string reviewerList
    pure $ MkPullRequest {
        number
      , createdAt
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

