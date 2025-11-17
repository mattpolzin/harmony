module FFI.GitHub

import Data.Promise
import Data.PullRequest
import Data.Issue
import Data.Review
import Data.String
import Data.String.Extra
import Data.User
import Data.Vect
import FFI
import JSON.Parser
import Language.JSON.Accessors

import public Data.Fin

%default total

okit_ffi : (fnName : String) -> String
okit_ffi = node_ffi "okit"

data OctokitRef : Type

export
data Octokit = Kit (Ptr OctokitRef)

export
data OctokitGraphQlId = GQLId String

%foreign okit_ffi "octokit"
prim__octokit : (authToken : String) -> PrimIO (Ptr OctokitRef)

export
octokit : (authToken : String) -> IO Octokit
octokit authToken = Kit <$> (primIO $ prim__octokit authToken)

data ErrorCode = NotFound
               | InternalError
               | OtherStatus Integer

record OctokitError where
  constructor MkError
  code : ErrorCode
  error : String

internalError : String -> OctokitError
internalError = MkError InternalError

statusCode : Integer -> ErrorCode
statusCode code =
  if code == 404
     then NotFound
     else OtherStatus code

parseError : String -> Either OctokitError OctokitError
parseError str = do
  json <- mapFst unexpectedPayload $ parseJSON Virtual str
  mapFst internalError $ parse json

  where
    unexpectedPayload : a -> OctokitError
    unexpectedPayload = const $ internalError "Unexpected error JSON: \{str}"

    parse : JSON -> Either String OctokitError
    parse (JObject err) =
      do [status, msg] <- lookupAll ["status", "error"] err
         s <- integer status
         e <- string msg
         pure $ MkError (statusCode s) e
    parse _ = Left "Expected an Object containing error and status keys"

ignoreStatus : Promise String a -> Promise String a
ignoreStatus = mapError (errMsg . parseError)
  where
    errMsg : Either OctokitError OctokitError -> String
    errMsg = either (.error) (.error)

parseOctokitError : String -> OctokitError
parseOctokitError = either id id . parseError

public export
data OrgError = NotAnOrg | Msg String

-- for non-orgs (i.e. individual users) GitHub 404s on API
-- routes that are only supported for orgs.
orgError : OctokitError -> OrgError
orgError (MkError NotFound error) = NotAnOrg
orgError (MkError InternalError error) = Msg error
orgError (MkError (OtherStatus i) error) = Msg error

mapOrgError : Promise String a -> Promise OrgError a
mapOrgError = mapError (orgError . parseOctokitError)

parsePrimResult : (parser : String -> Either String a)
               -> (action : (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ())
               -> Promise String a
parsePrimResult parser action = either . parser =<< (ignoreStatus $ promiseIO action)

%foreign okit_ffi "get_repo_default_branch"
prim__getRepoDefaultBranch : Ptr OctokitRef
                          -> (owner : String)
                          -> (repo : String)
                          -> (onSuccess : String -> PrimIO ())
                          -> (onFailure : String -> PrimIO ())
                          -> PrimIO ()

export
getRepoDefaultBranch : Octokit =>
                       (owner : String)
                    -> (repo : String)
                    -> Promise String String
getRepoDefaultBranch @{Kit ptr} owner repo = ignoreStatus . promiseIO $ prim__getRepoDefaultBranch ptr owner repo

%foreign okit_ffi "list_repo_labels"
prim__listRepoLabels : Ptr OctokitRef
                    -> (owner : String)
                    -> (repo : String)
                    -> (onSuccess : String -> PrimIO ())
                    -> (onFailure : String -> PrimIO ())
                    -> PrimIO ()

export
listRepoLabels : Octokit =>
                 (owner : String)
              -> (repo : String)
              -> Promise String (List String)
listRepoLabels @{Kit ptr} owner repo =
  lines <$> (ignoreStatus . promiseIO $ prim__listRepoLabels ptr owner repo)

%foreign okit_ffi "list_teams"
prim__listTeams : Ptr OctokitRef
               -> (org : String) 
               -> (onSuccess : String -> PrimIO ()) 
               -> (onFailure : String -> PrimIO ()) 
               -> PrimIO ()

export
listTeams : Octokit => (org : String) -> Promise OrgError (List String)
listTeams @{Kit ptr} org = 
  lines <$> (mapOrgError . promiseIO $ prim__listTeams ptr org)

export
forceListTeams : Octokit =>
                 (org : String)
              -> Promise' (List String)
forceListTeams = mapError errString . listTeams
  where
    errString : OrgError -> String
    errString NotAnOrg = "You can only list teams for repositories belonging to GitHub organizations"
    errString (Msg str) = str

%foreign okit_ffi "list_my_teams"
prim__listMyTeams : Ptr OctokitRef
                 -> (onSuccess : String -> PrimIO ())
                 -> (onFailure : String -> PrimIO ())
                 -> PrimIO ()

export
listMyTeams : Octokit => Promise String (List String)
listMyTeams @{Kit ptr} =
  lines <$> (ignoreStatus . promiseIO $ prim__listMyTeams ptr)

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
                -> Promise String (List PullRequest)
listPRsForBranch @{Kit ptr} owner repo branch = 
  parsePrimResult parsePullRequestsString $
    prim__listPRsForBranch ptr owner repo branch

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
        -> Promise String PullRequest
createPR @{Kit ptr} {isDraft} owner repo head base title description =
  parsePrimResult parsePullRequestString $
    prim__createPR ptr owner repo head base title description isDraft

%foreign okit_ffi "create_issue"
prim__createIssue : Ptr OctokitRef 
                 -> (owner : String) 
                 -> (repo : String) 
                 -> (title : String) 
                 -> (body : String) 
                 -> (onSuccess : String -> PrimIO ()) 
                 -> (onFailure : String -> PrimIO ()) 
                 -> PrimIO ()

export
createIssue : Octokit => 
            (owner : String) 
         -> (repo : String) 
         -> (title : String) 
         -> (body : String) 
         -> Promise String Issue
createIssue @{Kit ptr} owner repo title body =
  parsePrimResult parseIssueString $
    prim__createIssue ptr owner repo title body

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
             -> Promise String ()
createComment @{Kit ptr} owner repo issueOrPrNumber message =
  ignore . ignoreStatus . promiseIO $ prim__createComment ptr owner repo issueOrPrNumber message

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
                 -> Promise String (List String)
listPullReviewers @{Kit ptr} owner repo stateFilter pageLimit = 
  lines <$> (ignoreStatus . promiseIO $ prim__listPullReviewers ptr owner repo (pullRequestStateFilter stateFilter) (cast $ finToNat pageLimit))

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
                       -> Promise String String
listPullRequestsJsonStr @{Kit ptr} owner repo stateFilter pageLimit {page} = 
  let filter  = pullRequestStateFilter stateFilter
      pgLimit = cast $ finToNat pageLimit
      pg      = cast (S page)
  in  ignoreStatus . promiseIO $ prim__listPullRequests ptr owner repo filter pgLimit pg

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
                -> Promise String (List PullRequest)
listPullRequests @{Kit ptr} owner repo stateFilter pageLimit {page} = 
  either . parsePullRequestsString =<< listPullRequestsJsonStr owner repo stateFilter pageLimit {page}

%foreign okit_ffi "get_pr_graphql_id"
prim__getPullRequestGraphQlId : Ptr OctokitRef 
                             -> (owner : String) 
                             -> (repo : String) 
                             -> (pullNumber : Integer) 
                             -> (onSuccess : String -> PrimIO ()) 
                             -> (onFailure : String -> PrimIO ())
                             -> PrimIO ()

export
getPullRequestGraphQlIdStr : Octokit =>
                             (owner : String) 
                          -> (repo : String) 
                          -> (pullNumber : Integer) 
                          -> Promise String String
getPullRequestGraphQlIdStr @{Kit ptr} owner repo pullNumber = 
  ignoreStatus . promiseIO $ prim__getPullRequestGraphQlId ptr owner repo pullNumber

||| Get a Pull Request GraphQL Id.
||| This is an opaque value only needed by select Harmony FFI functions
||| currently.
export
getPullRequestGraphQlId : Octokit =>
                           (owner : String) 
                        -> (repo : String) 
                        -> (pullNumber : Integer) 
                        -> Promise String OctokitGraphQlId 
getPullRequestGraphQlId @{Kit ptr} owner repo pullNumber = 
  mapSnd GQLId . ignoreStatus . promiseIO $ prim__getPullRequestGraphQlId ptr owner repo pullNumber

%foreign okit_ffi "mark_pr_draft"
prim__markPullRequestDraft : Ptr OctokitRef 
                      -> (opaqueGraphQlId : String) 
                      -> (onSuccess : String -> PrimIO ()) 
                      -> (onFailure : String -> PrimIO ()) 
                      -> PrimIO ()

||| Mark a Pull Request as a draft
||| This function needs a GraphQL Id instead of a pull number.
||| See `getPullRequestGraphQlId`.
export
markPullRequestDraft : Octokit => 
                       (graphQlId : OctokitGraphQlId) 
                    -> Promise String PullRequest
markPullRequestDraft @{Kit ptr} (GQLId id) = do
  parsePrimResult parsePullRequestString $
    prim__markPullRequestDraft ptr id

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
||| team review request and pick someone from it whereas if a team
||| and some individuals are requested in one go then GitHub will
||| never apply its round-robin or weight-balanced selection but
||| instead will leave the team itself requested.
export
addPullReviewers : Octokit => 
                   (owner : String) 
                -> (repo : String) 
                -> (pullNumber : Integer) 
                -> (reviewers : List String) 
                -> (teamReviewers : List String) 
                -> Promise String (List String)
addPullReviewers @{Kit ptr} owner repo pullNumber reviewers teamReviewers = do
  teamReviewers <- lines <$> (ignoreStatus . promiseIO $ prim__addPullReviewers ptr owner repo pullNumber "" (join "," teamReviewers))
  individualReviewers <- lines <$> (ignoreStatus . promiseIO $ prim__addPullReviewers ptr owner repo pullNumber (join "," reviewers) "")
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
             -> Promise String (List String)
addPullLabels @{Kit ptr} owner repo pullNumber labels =
  lines <$> (ignoreStatus . promiseIO $ prim__addLabels ptr owner repo pullNumber (join "," labels))

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
                      -> Promise String String
listPullReviewsJsonStr @{Kit ptr} owner repo pullNumber =
  ignoreStatus . promiseIO $ prim__listPullReviews ptr owner repo pullNumber

export
listPullReviews : Octokit => 
                  (owner : String) 
               -> (repo : String) 
               -> (pullNumber : Integer) 
               -> Promise String (List Review)
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
               -> Promise OrgError (List String)
listTeamMembers @{Kit ptr} org teamSlug = 
  lines <$> (mapOrgError . promiseIO $ prim__listTeamMembers ptr org teamSlug)

export
forceListTeamMembers : Octokit =>
                       (org : String)
                    -> (teamSlug : String)
                    -> Promise' (List String)
forceListTeamMembers = mapError errString .: listTeamMembers
  where
    errString : OrgError -> String
    errString NotAnOrg = "You can only use teams with repositories belonging to GitHub organizations"
    errString (Msg str) = str

%foreign okit_ffi "list_org_members"
prim__listOrgMembers : Ptr OctokitRef 
                    -> (org : String) 
                    -> (onSuccess : String -> PrimIO ()) 
                    -> (onFailure : String -> PrimIO ()) 
                    -> PrimIO ()

export
listOrgMembers : Octokit => (org : String) -> Promise OrgError (List String)
listOrgMembers @{Kit ptr} org =
  lines <$> (mapOrgError . promiseIO $ prim__listOrgMembers ptr org)

%foreign okit_ffi "get_user"
prim__getUser : Ptr OctokitRef 
             -> (username : String) 
             -> (onSuccess : String -> PrimIO ()) 
             -> (onFailure : String -> PrimIO ()) 
             -> PrimIO ()

export
getUser : Octokit => (username : String) -> Promise String User
getUser @{Kit ptr} = parsePrimResult parseUserString . prim__getUser ptr

%foreign okit_ffi "get_self"
prim__getSelf : Ptr OctokitRef 
             -> (onSuccess : String -> PrimIO ()) 
             -> (onFailure : String -> PrimIO ()) 
             -> PrimIO ()

export
getSelf : Octokit => Promise String User
getSelf @{Kit ptr} = parsePrimResult parseUserString $ prim__getSelf ptr

