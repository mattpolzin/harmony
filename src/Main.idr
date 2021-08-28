module Main

import Data.Either
import Data.List
import Data.Promise
import Data.String
import Language.JSON
import System

data OctokitRef : Type

data Octokit = Kit (Ptr OctokitRef)

%foreign "node:support:octokit,okit"
prim__octokit : (authToken : String) -> PrimIO (Ptr OctokitRef)

octokit : (authToken : String) -> IO Octokit
octokit authToken = Kit <$> (primIO $ prim__octokit authToken)

promiseIO : (primFn : (String -> PrimIO ()) -> (String -> PrimIO ()) -> PrimIO ()) -> Promise String
promiseIO primFn = 
  promisify $ \ok,notOk => primFn (\res => toPrim $ ok res) (\err => toPrim $ notOk err)

data PullRequestState = Open | Closed

Show PullRequestState where
  show Open = "open"
  show Closed = "closed"

pullRequestStateFilter : Maybe PullRequestState -> String
pullRequestStateFilter Nothing = "all"
pullRequestStateFilter (Just s) = show s

%foreign "node:support:list_reviewers,okit"
prim__listPullReviewers : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (stateFilter : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listPullReviewers : Octokit => (owner : String) -> (repo : String) -> (stateFilter : Maybe PullRequestState) -> Promise (List String)
listPullReviewers @{(Kit ptr)} owner repo stateFilter = 
  lines <$> (promiseIO $ prim__listPullReviewers ptr owner repo (pullRequestStateFilter stateFilter))

%foreign "node:support:list_team_members,okit"
prim__listTeamMembers : Ptr OctokitRef -> (org : String) -> (teamSlug : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listTeamMembers : Octokit => (org : String) -> (teamSlug : String) -> Promise (List String)
listTeamMembers @{(Kit ptr)} org teamSlug = 
  lines <$> (promiseIO $ prim__listTeamMembers ptr org teamSlug)

exitError : HasIO io => String -> io ()
exitError err =
  do putStrLn err
     exitFailure

tmp : HasIO io => (List String, List String) -> io ()
tmp (x, y) =
  do print x
     print y

Timestamp : Type
Timestamp = Bits32

record Config where
  updatedAt : Timestamp
  org : String
  repo : String
  teamSlugs : List String

loadConfig : HasIO io => io Config

main : IO ()
main =
  do Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     config <- loadConfig
     (Kit kit) <- octokit pat
     resolve' tmp exitError $
       do pullReviewers <- listPullReviewers config.org config.repo Nothing
          teamMembers   <- maybe (pure []) (listTeamMembers config.org) $ head' config.teamSlugs
          pure (pullReviewers, teamMembers)

