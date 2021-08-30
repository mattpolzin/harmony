module FFI.GitHub

import Data.Vect
import Data.Promise
import Data.PullRequest
import Data.String
import Data.String.Extra
import FFI
import Language.JSON
import Language.JSON.Accessors

%default total

okit_ffi : (fnName : String) -> String
okit_ffi = node_ffi "okit"

data OctokitRef : Type

export
data Octokit = Kit (Ptr OctokitRef)

%foreign okit_ffi "octokit"
prim__octokit : (authToken : String) -> PrimIO (Ptr OctokitRef)

export
octokit : (authToken : String) -> IO Octokit
octokit authToken = Kit <$> (primIO $ prim__octokit authToken)

%foreign okit_ffi "list_teams"
prim__listTeams : Ptr OctokitRef -> (org : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listTeams : Octokit => (org : String) -> Promise (List String)
listTeams @{(Kit ptr)} org = 
  lines <$> (promiseIO $ prim__listTeams ptr org)

%foreign okit_ffi "list_pr_numbers"
prim__listPRsForBranch : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (branch : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listPRsForBranch : Octokit => (owner : String) -> (repo : String) -> (branch : String) -> Promise (List PullRequest)
listPRsForBranch @{(Kit ptr)} owner repo branch = 
  do Just json <- JSON.parse <$> (promiseIO $ prim__listPRsForBranch ptr owner repo branch)
       | Nothing => reject "Could not parse Pull Request JSON."
     prs <- either $ array json Right
     traverse parse' prs
       where
         parse' : JSON -> Promise PullRequest
         parse' json = either $ 
           do pr <- object json
              [pullNumber, authorLogin] <- lookupAll ["pull_number", "author"] pr
              number <- integer pullNumber
              author <- string authorLogin
              pure $ MkPullRequest {
                  number
                , author
                }

public export
data PullRequestState = Open | Closed

export
Show PullRequestState where
  show Open = "open"
  show Closed = "closed"

pullRequestStateFilter : Maybe PullRequestState -> String
pullRequestStateFilter Nothing = "all"
pullRequestStateFilter (Just s) = show s

%foreign okit_ffi "list_reviewers"
prim__listPullReviewers : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (stateFilter : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listPullReviewers : Octokit => (owner : String) -> (repo : String) -> (stateFilter : Maybe PullRequestState) -> Promise (List String)
listPullReviewers @{(Kit ptr)} owner repo stateFilter = 
  lines <$> (promiseIO $ prim__listPullReviewers ptr owner repo (pullRequestStateFilter stateFilter))

-- reviewers and teamReviewers should be comma separated values encoded in a string.
%foreign okit_ffi "add_reviewers"
prim__addPullReviewers : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (pullNumber : Integer) -> (reviewers : String) -> (teamReviewers : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

||| Add reviewers to a Pull Request.
|||
||| onSuccess will receive a newline delimited list of
||| all reviewers of the modified PR.
export
addPullReviewers : Octokit => (owner : String) -> (repo : String) -> (pullNumber : Integer) -> (reviewers : List String) -> (teamReviewers : List String) -> Promise (List String)
addPullReviewers @{(Kit ptr)} owner repo pullNumber reviewers teamReviewers = 
  lines <$> (promiseIO $ prim__addPullReviewers ptr owner repo pullNumber (join "," reviewers) (join "," teamReviewers))

%foreign okit_ffi "list_team_members"
prim__listTeamMembers : Ptr OctokitRef -> (org : String) -> (teamSlug : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listTeamMembers : Octokit => (org : String) -> (teamSlug : String) -> Promise (List String)
listTeamMembers @{(Kit ptr)} org teamSlug = 
  lines <$> (promiseIO $ prim__listTeamMembers ptr org teamSlug)

