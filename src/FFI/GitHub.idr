module FFI.GitHub

import Data.Promise
import Data.PullRequest
import Data.Review
import Data.String
import Data.String.Extra
import Data.User
import Data.Vect
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
prim__getRepoDefaultBranch : Ptr OctokitRef
                          -> (org : String)
                          -> (repo : String)
                          -> (onSuccess : String -> PrimIO ())
                          -> (onFailure : String -> PrimIO ())
                          -> PrimIO ()

export
getRepoDefaultBranch : Octokit =>
                       (org : String)
                    -> (repo : String)
                    -> Promise String
getRepoDefaultBranch @{Kit ptr} org repo = promiseIO $ prim__getRepoDefaultBranch ptr org repo

%foreign okit_ffi "list_repo_labels"
prim__listRepoLabels : Ptr OctokitRef
                    -> (org : String)
                    -> (repo : String)
                    -> (onSuccess : String -> PrimIO ())
                    -> (onFailure : String -> PrimIO ())
                    -> PrimIO ()

export
listRepoLabels : Octokit =>
                 (org : String)
              -> (repo : String)
              -> Promise (List String)
listRepoLabels @{Kit ptr} org repo =
  lines <$> (promiseIO $ prim__listRepoLabels ptr org repo)

%foreign okit_ffi "list_teams"
prim__listTeams : Ptr OctokitRef
               -> (org : String) 
               -> (onSuccess : String -> PrimIO ()) 
               -> (onFailure : String -> PrimIO ()) 
               -> PrimIO ()

export
listTeams : Octokit => (org : String) -> Promise (List String)
listTeams @{Kit ptr} org = 
  lines <$> (promiseIO $ prim__listTeams ptr org)

%foreign okit_ffi "list_my_teams"
prim__listMyTeams : Ptr OctokitRef
                 -> (onSuccess : String -> PrimIO ())
                 -> (onFailure : String -> PrimIO ())
                 -> PrimIO ()

export
listMyTeams : Octokit => Promise (List String)
listMyTeams @{Kit ptr} =
  lines <$> (promiseIO $ prim__listMyTeams ptr)

%foreign okit_ffi "list_pull_requests_for_branch"
prim__listPRsForBranch : Ptr OctokitRef 
                      -> (owner : String) 
                      -> (repo : String) 
                      -> (branch : String) 
                      -> (onSuccess : String -> PrimIO ()) 
                      -> (onFailure : String -> PrimIO ()) 
                      -> PrimIO ()

export
listPRsForBranch : Octokit => 
                   (owner : String) 
                -> (repo : String) 
                -> (branch : String) 
                -> Promise (List PullRequest)
listPRsForBranch @{Kit ptr} owner repo branch = 
  either . parsePullRequestsString =<< (promiseIO $ prim__listPRsForBranch ptr owner repo branch)

%foreign okit_ffi "create_pr"
prim__createPR : Ptr OctokitRef 
              -> (owner : String) 
              -> (repo : String) 
              -> (head : String) 
              -> (base : String) 
              -> (title : String) 
              -> (body : String) 
              -> (isDraft : Bool)
              -> (onSuccess : String -> PrimIO ()) 
              -> (onFailure : String -> PrimIO ()) 
              -> PrimIO ()

export
createPR : Octokit => 
           {default False isDraft : Bool}
        -> (owner : String) 
        -> (repo : String) 
        -> (head : String) 
        -> (base : String) 
        -> (title : String) 
        -> (description : String) 
        -> Promise PullRequest
createPR @{Kit ptr} {isDraft} owner repo head base title description =
  either . parsePullRequestString =<< (promiseIO $ prim__createPR ptr owner repo head base title description isDraft)

%foreign okit_ffi "create_comment"
prim__createComment : Ptr OctokitRef 
                   -> (owner : String) 
                   -> (repo : String) 
                   -> (issueOrPrNumber : Integer) 
                   -> (message : String) 
                   -> (onSuccess : String -> PrimIO ()) 
                   -> (onFailure : String -> PrimIO ()) 
                   -> PrimIO ()

export
createComment : Octokit => 
                (owner : String) 
             -> (repo : String) 
             -> (issueOrPrNumber : Integer) 
             -> (message : String) 
             -> Promise ()
createComment @{Kit ptr} owner repo issueOrPrNumber message =
  ignore . promiseIO $ prim__createComment ptr owner repo issueOrPrNumber message

Show GitHubPRState where
  show Open   = "open"
  show Closed = "closed"

pullRequestStateFilter : Maybe GitHubPRState -> String
pullRequestStateFilter Nothing = "all"
pullRequestStateFilter (Just s) = show s

%foreign okit_ffi "list_reviewers"
prim__listPullReviewers : Ptr OctokitRef 
                       -> (owner : String) 
                       -> (repo : String) 
                       -> (stateFilter : String) 
                       -> (pageLimit : Int16) 
                       -> (onSuccess : String -> PrimIO ()) 
                       -> (onFailure : String -> PrimIO ()) 
                       -> PrimIO ()

||| List 1 page of pull reviewers (a list of usernames, one per user per review).
export
listPullReviewers : Octokit => 
                    (owner : String) 
                 -> (repo : String) 
                 -> (stateFilter : Maybe GitHubPRState) 
                 -> (pageLimit : Fin 101) 
                 -> Promise (List String)
listPullReviewers @{Kit ptr} owner repo stateFilter pageLimit = 
  lines <$> (promiseIO $ prim__listPullReviewers ptr owner repo (pullRequestStateFilter stateFilter) (cast $ finToNat pageLimit))

%foreign okit_ffi "list_pull_requests"
prim__listPullRequests : Ptr OctokitRef 
                      -> (owner : String) 
                      -> (repo : String) 
                      -> (stateFilter : String) 
                      -> (pageLimit : Int16) 
                      -> (page : Int16) 
                      -> (onSuccess : String -> PrimIO ()) 
                      -> (onFailure : String -> PrimIO ())
                      -> PrimIO ()

export
listPullRequestsJsonStr : Octokit =>
                          (owner : String) 
                       -> (repo : String) 
                       -> (stateFilter : Maybe GitHubPRState) 
                       -> (pageLimit : Fin 101) 
                       -> {default 0 page : Nat}
                       -> Promise String
listPullRequestsJsonStr @{Kit ptr} owner repo stateFilter pageLimit {page} = 
  let filter  = pullRequestStateFilter stateFilter
      pgLimit = cast $ finToNat pageLimit
      pg      = cast (S page)
  in  promiseIO $ prim__listPullRequests ptr owner repo filter pgLimit pg

||| List the most recent pull requests by creation date.
|||
||| @owner       The repository owner (a.k.a. org name).
||| @repo        The repository name.
||| @stateFilter Retrieve only open or only closed PRs. Pass Nothing to retrieve all PRs.
||| @pageLimit   The number of results per page (max 100).
||| @page        The zero-indexed page index to retieve. Defaults to 0.
export
listPullRequests : Octokit => 
                   (owner : String) 
                -> (repo : String) 
                -> (stateFilter : Maybe GitHubPRState) 
                -> (pageLimit : Fin 101) 
                -> {default 0 page : Nat}
                -> Promise (List PullRequest)
listPullRequests @{Kit ptr} owner repo stateFilter pageLimit {page} = 
  either . parsePullRequestsString =<< listPullRequestsJsonStr owner repo stateFilter pageLimit {page}

-- reviewers and teamReviewers should be comma separated values encoded in a string.
%foreign okit_ffi "add_reviewers"
prim__addPullReviewers : Ptr OctokitRef 
                      -> (owner : String) 
                      -> (repo : String) 
                      -> (pullNumber : Integer) 
                      -> (reviewers : String) 
                      -> (teamReviewers : String) 
                      -> (onSuccess : String -> PrimIO ()) 
                      -> (onFailure : String -> PrimIO ()) 
                      -> PrimIO ()

||| Add reviewers to a Pull Request.
|||
||| Will produce a list of applied reviewers.
|||
||| Applies team reviewers first as one API request and then
||| individual reviewers second as a second API request. This
||| implementation detail forces (or allows) GitHub to take a
||| team assignment and pick someone from it whereas if a team
||| and some individuals are assigned in one go then GitHub will
||| never apply its round-robin or weight-balanced selection but
||| instead will leave the team itself assigned.
export
addPullReviewers : Octokit => 
                   (owner : String) 
                -> (repo : String) 
                -> (pullNumber : Integer) 
                -> (reviewers : List String) 
                -> (teamReviewers : List String) 
                -> Promise (List String)
addPullReviewers @{Kit ptr} owner repo pullNumber reviewers teamReviewers = do
  teamReviewers <- lines <$> (promiseIO $ prim__addPullReviewers ptr owner repo pullNumber "" (join "," teamReviewers))
  individualReviewers <- lines <$> (promiseIO $ prim__addPullReviewers ptr owner repo pullNumber (join "," reviewers) "")
  pure $ teamReviewers ++ individualReviewers

%foreign okit_ffi "add_labels"
prim__addLabels : Ptr OctokitRef
               -> (owner : String)
               -> (repo : String)
               -> (pullNumber : Integer)
               -> (labels : String)
               -> (onSuccess : String -> PrimIO ())
               -> (onFailure : String -> PrimIO ())
               -> PrimIO ()

||| Add labels to a Pull Request.
|||
||| Will produce a list of all labels
||| (both new & previously applied).
export
addPullLabels : Octokit =>
                (owner : String)
             -> (repo : String)
             -> (pullNumber : Integer)
             -> (labels : List String)
             -> Promise (List String)
addPullLabels @{Kit ptr} owner repo pullNumber labels =
  lines <$> (promiseIO $ prim__addLabels ptr owner repo pullNumber (join "," labels))

%foreign okit_ffi "list_pr_reviews"
prim__listPullReviews : Ptr OctokitRef 
                     -> (owner : String) 
                     -> (repo : String) 
                     -> (pullNumber : Integer) 
                     -> (onSuccess : String -> PrimIO ()) 
                     -> (onFailure : String -> PrimIO ()) 
                     -> PrimIO ()

export
listPullReviewsJsonStr : Octokit => 
                         (owner : String) 
                      -> (repo : String) 
                      -> (pullNumber : Integer) 
                      -> Promise String
listPullReviewsJsonStr @{Kit ptr} owner repo pullNumber =
  promiseIO $ prim__listPullReviews ptr owner repo pullNumber

export
listPullReviews : Octokit => 
                  (owner : String) 
               -> (repo : String) 
               -> (pullNumber : Integer) 
               -> Promise (List Review)
listPullReviews owner repo pullNumber =
  either . parseReviewsString =<< listPullReviewsJsonStr owner repo pullNumber

%foreign okit_ffi "list_team_members"
prim__listTeamMembers : Ptr OctokitRef 
                     -> (org : String) 
                     -> (teamSlug : String) 
                     -> (onSuccess : String -> PrimIO ()) 
                     -> (onFailure : String -> PrimIO ()) 
                     -> PrimIO ()

export
listTeamMembers : Octokit => 
                  (org : String) 
               -> (teamSlug : String) 
               -> Promise (List String)
listTeamMembers @{Kit ptr} org teamSlug = 
  lines <$> (promiseIO $ prim__listTeamMembers ptr org teamSlug)

%foreign okit_ffi "list_org_members"
prim__listOrgMembers : Ptr OctokitRef 
                    -> (org : String) 
                    -> (onSuccess : String -> PrimIO ()) 
                    -> (onFailure : String -> PrimIO ()) 
                    -> PrimIO ()

export
listOrgMembers : Octokit => (org : String) -> Promise (List String)
listOrgMembers @{Kit ptr} org =
  lines <$> (promiseIO $ prim__listOrgMembers ptr org)

%foreign okit_ffi "get_user"
prim__getUser : Ptr OctokitRef 
             -> (username : String) 
             -> (onSuccess : String -> PrimIO ()) 
             -> (onFailure : String -> PrimIO ()) 
             -> PrimIO ()

export
getUser : Octokit => (username : String) -> Promise User
getUser @{Kit ptr} = either . parseUserString <=< promiseIO . prim__getUser ptr

%foreign okit_ffi "get_self"
prim__getSelf : Ptr OctokitRef 
             -> (onSuccess : String -> PrimIO ()) 
             -> (onFailure : String -> PrimIO ()) 
             -> PrimIO ()

export
getSelf : Octokit => Promise User
getSelf @{Kit ptr} = either . parseUserString =<< promiseIO (prim__getSelf ptr)

