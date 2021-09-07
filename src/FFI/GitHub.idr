module FFI.GitHub

import Data.Vect
import Data.Promise
import Data.PullRequest
import Data.String
import Data.String.Extra
import FFI
import Language.JSON
import Language.JSON.Accessors

import public Data.Fin

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

%foreign okit_ffi "get_repo_default_branch"
prim__getRepoDefaultBranch : Ptr OctokitRef -> (org : String) -> (repo : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
getRepoDefaultBranch : Octokit => (org : String) -> (repo : String) -> Promise String
getRepoDefaultBranch @{(Kit ptr)} org repo = promiseIO $ prim__getRepoDefaultBranch ptr org repo

%foreign okit_ffi "list_teams"
prim__listTeams : Ptr OctokitRef -> (org : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listTeams : Octokit => (org : String) -> Promise (List String)
listTeams @{(Kit ptr)} org = 
  lines <$> (promiseIO $ prim__listTeams ptr org)

%foreign okit_ffi "list_prs"
prim__listPRsForBranch : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (branch : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

parsePR : JSON -> Promise PullRequest
parsePR json = either $ 
 do pr <- object json
    [pullNumber, authorLogin] <- lookupAll ["pull_number", "author"] pr
    number <- integer pullNumber
    author <- string authorLogin
    pure $ MkPullRequest {
        number
      , author
      }

export
listPRsForBranch : Octokit => (owner : String) -> (repo : String) -> (branch : String) -> Promise (List PullRequest)
listPRsForBranch @{(Kit ptr)} owner repo branch = 
  do Just json <- JSON.parse <$> (promiseIO $ prim__listPRsForBranch ptr owner repo branch)
       | Nothing => reject "Could not parse Pull Request JSON."
     prs <- either $ array Right json 
     traverse parsePR prs

%foreign okit_ffi "create_pr"
prim__createPR : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (head : String) -> (base : String) -> (title : String) -> (body : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
createPR : Octokit => (owner : String) -> (repo : String) -> (head : String) -> (base : String) -> (title : String) -> (description : String) -> Promise PullRequest
createPR @{(Kit ptr)} owner repo head base title description =
  do Just json <- JSON.parse <$> (promiseIO $ prim__createPR ptr owner repo head base title description)
       | Nothing => reject "Could not parse Pull Request JSON."
     parsePR json

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
prim__listPullReviewers : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (stateFilter : String) -> (pageLimit : Int16) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listPullReviewers : Octokit => (owner : String) -> (repo : String) -> (stateFilter : Maybe PullRequestState) -> (pageLimit : Fin 100) -> Promise (List String)
listPullReviewers @{(Kit ptr)} owner repo stateFilter pageLimit = 
  lines <$> (promiseIO $ prim__listPullReviewers ptr owner repo (pullRequestStateFilter stateFilter) (cast $ finToNat pageLimit))

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

%foreign okit_ffi "list_org_members"
prim__listOrgMembers : Ptr OctokitRef -> (org : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
listOrgMembers : Octokit => (org : String) -> Promise (List String)
listOrgMembers @{(Kit ptr)} org =
  lines <$> (promiseIO $ prim__listOrgMembers ptr org)


