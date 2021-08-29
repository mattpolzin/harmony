module Main

import Data.Either
import Data.List
import Data.Promise
import Data.String
import Data.Vect
import Language.JSON
import System
import System.File

%default total

okit_ffi : (fnName : String) -> String
okit_ffi fnName = "node:support:\{fnName},okit"

data OctokitRef : Type

data Octokit = Kit (Ptr OctokitRef)

%foreign okit_ffi "octokit"
prim__octokit : (authToken : String) -> PrimIO (Ptr OctokitRef)

octokit : (authToken : String) -> IO Octokit
octokit authToken = Kit <$> (primIO $ prim__octokit authToken)

promiseIO : (primFn : (String -> PrimIO ()) -> (String -> PrimIO ()) -> PrimIO ()) -> Promise String
promiseIO primFn = 
  promisify $ \ok,notOk => primFn (\res => toPrim $ ok res) (\err => toPrim $ notOk err)

%foreign okit_ffi "list_teams"
prim__listTeams : Ptr OctokitRef -> (org : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listTeams : Octokit => (org : String) -> Promise (List String)
listTeams @{(Kit ptr)} org = 
  lines <$> (promiseIO $ prim__listTeams ptr org)

%foreign okit_ffi "list_pr_numbers"
prim__listPullNumbersForBranch : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (branch : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listPullNumbersForBranch : Octokit => (owner : String) -> (repo : String) -> (branch : String) -> Promise (List String)
listPullNumbersForBranch @{(Kit ptr)} owner repo branch = 
  lines <$> (promiseIO $ prim__listPullNumbersForBranch ptr owner repo branch)

data PullRequestState = Open | Closed

Show PullRequestState where
  show Open = "open"
  show Closed = "closed"

pullRequestStateFilter : Maybe PullRequestState -> String
pullRequestStateFilter Nothing = "all"
pullRequestStateFilter (Just s) = show s

%foreign okit_ffi "list_reviewers"
prim__listPullReviewers : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (stateFilter : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listPullReviewers : Octokit => (owner : String) -> (repo : String) -> (stateFilter : Maybe PullRequestState) -> Promise (List String)
listPullReviewers @{(Kit ptr)} owner repo stateFilter = 
  lines <$> (promiseIO $ prim__listPullReviewers ptr owner repo (pullRequestStateFilter stateFilter))

%foreign okit_ffi "list_team_members"
prim__listTeamMembers : Ptr OctokitRef -> (org : String) -> (teamSlug : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listTeamMembers : Octokit => (org : String) -> (teamSlug : String) -> Promise (List String)
listTeamMembers @{(Kit ptr)} org teamSlug = 
  lines <$> (promiseIO $ prim__listTeamMembers ptr org teamSlug)

exitError : HasIO io => String -> io a
exitError err =
  do putStrLn err
     exitFailure

tmp : HasIO io => (List String, List String, List String, List String) -> io ()
tmp (w, x, y, z) =
  do printLn w
     printLn x
     printLn y
     printLn z

Timestamp : Type
Timestamp = Bits32

record Config where
  constructor MkConfig
  updatedAt : Timestamp
  org : String
  repo : String
  teamSlugs : List String

Show Config where
  show config = unlines [
      "updatedAt: \{show config.updatedAt}"
    , "org: \{show config.org}"
    , "repo: \{show config.repo}"
    , "teamSlugs: \{show config.teamSlugs}"
    ]

lookupAll : Vect n String -> List (String, JSON) -> Either String (Vect n JSON)
lookupAll [] dict            = Right []
lookupAll (key :: keys) dict = [| lookup' key dict :: lookupAll keys dict |]
  where
    lookup' : String -> List (String, a) -> Either String a
    lookup' key = maybeToEither "Missing required key: \{key}." . lookup key

string : JSON -> Either String String
string (JString x) = Right x
string json = Left "Expected a string but found \{show json}."

integer : JSON -> Either String Integer
integer (JNumber x) = Right $ cast x
integer json = Left "Expected an integer but found \{show json}."

array : JSON -> (JSON -> Either String a) -> Either String (List a)
array (JArray xs) f = traverse f xs
array json f = Left "Expected an array but found \{show json}."

parseConfig : String -> Either String Config
parseConfig = (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parseConfigJson
  where
    parseConfigJson : JSON -> Either String Config
    parseConfigJson (JObject config) = do [updatedAt, org, repo, teamSlugs] <-
                                            lookupAll ["updatedAt", "org", "repo", "teamSlugs"] config
                                          ua <- cast <$> integer updatedAt
                                          o <- string org
                                          r <- string repo
                                          ts <- array teamSlugs string
                                          pure $ MkConfig {
                                              updatedAt = ua
                                            , org       = o
                                            , repo      = r
                                            , teamSlugs = ts
                                            }
    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

covering
loadConfig : HasIO io => io Config
loadConfig = 
  do Right configFile <- readFile "config.json"
       | Left err => exitError "Error loading config.json: \{show err}."
     case parseConfig configFile of
          Right config => pure config
          Left err => exitError err

covering
main : IO ()
main =
  do Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     config <- loadConfig
     print config
     (Kit kit) <- octokit pat
     resolve' tmp exitError $
       do pullReviewers <- listPullReviewers config.org config.repo Nothing
          teams <- listTeams config.org
          teamMembers   <- maybe (pure []) (listTeamMembers config.org) $ head' config.teamSlugs
          openPrs <- listPullNumbersForBranch config.org config.repo "current-branch-name-here"
          pure (pullReviewers, teams, teamMembers, openPrs)

