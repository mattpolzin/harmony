module Data.PullRequest

import Data.Config
import Data.Date
import Data.Either
import Data.List
import Data.String
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

%default total

namespace FFI
  public export
  data GitHubPRState = Open | Closed

  export
  Show GitHubPRState where
    show Open = "open"
    show Closed = "closed"

public export
data PRState = Open | Closed | Merged

export
Show PRState where
  show Open   = "open"
  show Closed = "closed"
  show Merged = "merged"

export
toGHState : PRState -> GitHubPRState
toGHState Open   = Open
toGHState Closed = Closed
toGHState Merged = Closed

isMerged : PRState -> Bool
isMerged Merged = True
isMerged _      = False

export
Eq PRState where
  Open   == Open   = True
  Closed == Closed = True
  Merged == Merged = True
  _ == _ = False

public export
record PullRequest where
  constructor MkPullRequest
  ||| The pull request's "number" (as seen in URIs referring to the PR).
  number      : Integer
  ||| The pull request's title
  title : String
  ||| When the PR was created.
  createdAt   : Date
  ||| Is the PR currently a "draft"?
  isDraft     : Bool
  ||| The `login` of the author of the pull request.
  author      : String
  ||| Open or Closed status.
  state       : PRState
  ||| A List of all reviewers requested on the PR.
  reviewers   : List String
  ||| The branch being merged into some other branch.
  headRef     : String
  ||| The branch being merged into.
  baseRef     : String

%name PullRequest pr, pr1, pr2

export
Show PullRequest where
  show (MkPullRequest number _ _ _ author state _ headRef _) =
    "[\{show number}] (\{show state}) \{authorString} - headRef: \{headRef}"
    where
      authorString : String
      authorString = padRight 15 ' ' $ show author

export
webURI' : (org, repo : String) -> PullRequest -> String
webURI' org repo pr = "https://github.com/\{org}/\{repo}/pull/\{show pr.number}"

export
(.webURI) : Config => PullRequest -> String
pr.webURI @{config} = webURI' config.org config.repo pr

export
isAuthor : String -> PullRequest -> Bool
isAuthor login = (== login) . author

export
isRequestedReviewer : String -> PullRequest -> Bool
isRequestedReviewer login = any (== login) . reviewers

parseState : (merged : Bool) -> String -> Either String PRState
parseState False "open" = Right Open
parseState True  "open" = Left "Found a PR to be merged & open; this is a contradiction so something is wrong."
parseState False "closed" = Right Closed
parseState True  "closed" = Right Merged
parseState _ str = Left "Failed to parse a Pull Request State (open/closed). Found \{str}."

export
json : PullRequest -> JSON
json (MkPullRequest number title createdAt isDraft author state reviewers headRef baseRef) =
  JObject [
    ("pull_number", JInteger number)
  , ("title"      , JString title)
  , ("author"     , JString author)
  , ("state"      , JString (show $ toGHState state))
  , ("created_at" , JString (show createdAt))
  , ("merged"     , JBool $ isMerged state)
  , ("draft"      , JBool isDraft)
  , ("reviewers"  , JArray $ JString <$> reviewers)
  , ("head_ref"   , JString headRef)
  , ("base_ref"   , JString baseRef)
  ]

export
parsePR : JSON -> Either String PullRequest
parsePR json =
 do pr <- object json
    [pullNumber, pullTitle, authorLogin, stateStr, createdAtStr, mergedStr, isDraftStr, reviewerList, head, base] <- lookupAll ["pull_number", "title", "author", "state", "created_at", "merged", "draft", "reviewers", "head_ref", "base_ref"] pr
    number      <- integer pullNumber
    title <- string pullTitle
    author      <- string authorLogin
    merged      <- bool mergedStr
    isDraft     <- bool isDraftStr
    state       <- (parseState merged) =<< string stateStr
    createdAt   <- parseDateTime =<< string createdAtStr
    reviewers   <- array string reviewerList
    headRef     <- string head
    baseRef     <- string base
    pure $ MkPullRequest {
        number
      , title
      , createdAt
      , isDraft
      , author
      , state
      , reviewers
      , headRef
      , baseRef
      }

||| Parse a single pull request from a JSON String
export
parsePullRequestString : String -> Either String PullRequest
parsePullRequestString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parsePR

||| Parse a list of pull requests from a JSON String
export
parsePullRequestsString : String -> Either String (List PullRequest)
parsePullRequestsString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parsePR

