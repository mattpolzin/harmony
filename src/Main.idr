module Main

import Data.Bits
import Data.Either
import Data.List
import Data.Promise
import Data.String
import Data.String.Extra
import Data.Vect
import Language.JSON
import System
import System.File

%default total

-- JSON parsing help
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

object : JSON -> Either String (List (String, JSON))
object (JObject xs) = Right xs
object json = Left "Expected an object but found \{show json}."

-- Promise helper
promiseIO : (primFn : (String -> PrimIO ()) -> (String -> PrimIO ()) -> PrimIO ()) -> Promise String
promiseIO primFn = 
  promisify $ \ok,notOk => primFn (\res => toPrim $ ok res) (\err => toPrim $ notOk err)


-- FFIs
node_ffi : (libName : String) -> (fnName : String) -> String
node_ffi libName fnName = "node:support:\{fnName},\{libName}"

okit_ffi : (fnName : String) -> String
okit_ffi = node_ffi "okit"

git_ffi : (fnName : String) -> String
git_ffi = node_ffi "git"

data OctokitRef : Type

data Octokit = Kit (Ptr OctokitRef)

%foreign okit_ffi "octokit"
prim__octokit : (authToken : String) -> PrimIO (Ptr OctokitRef)

octokit : (authToken : String) -> IO Octokit
octokit authToken = Kit <$> (primIO $ prim__octokit authToken)

%foreign okit_ffi "list_teams"
prim__listTeams : Ptr OctokitRef -> (org : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listTeams : Octokit => (org : String) -> Promise (List String)
listTeams @{(Kit ptr)} org = 
  lines <$> (promiseIO $ prim__listTeams ptr org)

record PullRequest where
  constructor MkPullRequest
  ||| The pull request's "number" (as seen in URIs referring to the PR).
  number : Integer
  ||| The `login` of the author of the pull request.
  author : String

Show PullRequest where
  show (MkPullRequest number author) = "(\{show number}, \{show author})"

%foreign okit_ffi "list_pr_numbers"
prim__listPRsForBranch : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (branch : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

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

-- reviewers and teamReviewers should be comma separated values encoded in a string.
%foreign okit_ffi "add_reviewers"
prim__addPullReviewers : Ptr OctokitRef -> (owner : String) -> (repo : String) -> (pullNumber : Integer) -> (reviewers : String) -> (teamReviewers : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

||| Add reviewers to a Pull Request.
|||
||| onSuccess will receive a newline delimited list of
||| all reviewers of the modified PR.
addPullReviewers : Octokit => (owner : String) -> (repo : String) -> (pullNumber : Integer) -> (reviewers : List String) -> (teamReviewers : List String) -> Promise (List String)
addPullReviewers @{(Kit ptr)} owner repo pullNumber reviewers teamReviewers = 
  lines <$> (promiseIO $ prim__addPullReviewers ptr owner repo pullNumber (join "," reviewers) (join "," teamReviewers))

%foreign okit_ffi "list_team_members"
prim__listTeamMembers : Ptr OctokitRef -> (org : String) -> (teamSlug : String) -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

listTeamMembers : Octokit => (org : String) -> (teamSlug : String) -> Promise (List String)
listTeamMembers @{(Kit ptr)} org teamSlug = 
  lines <$> (promiseIO $ prim__listTeamMembers ptr org teamSlug)

data GitRef : Type

data Git = G (Ptr GitRef)

%foreign git_ffi "git"
prim__git : PrimIO (Ptr GitRef)

git : HasIO io => io Git
git = G <$> (primIO $ prim__git)

%foreign git_ffi "current_branch"
prim__currentBranch : Ptr GitRef -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

currentBranch : Git => Promise String
currentBranch @{(G ptr)} = promiseIO $ prim__currentBranch ptr

exitError : HasIO io => String -> io a
exitError err =
  do putStrLn err
     exitFailure

tmp : HasIO io => (List String, List String, List String) -> io ()
tmp (w, x, y) =
  do printLn w
     printLn x
     printLn y

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

json : Config -> JSON
json (MkConfig updatedAt org repo teamSlugs) = 
  JObject [
      ("org"      , JString org)
    , ("repo"     , JString repo)
    , ("teamSlugs", JArray $ JString <$> sort teamSlugs)
    , ("updatedAt", JNumber $ cast updatedAt)
    ]

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

createConfig : Octokit => Promise Config
createConfig = 
  do putStrLn "Creating a new configuration (storing in config.json)..."
     putStrLn "What GitHub org would you like to use harmony for?"
     org <- trim <$> getLine
     putStrLn "What repository would you like to use harmony for?"
     repo <- trim <$> getLine
     updatedAt <- cast <$> time
     do teamSlugs <- listTeams org
        let config = MkConfig {
            updatedAt
          , org
          , repo
          , teamSlugs
          }
        liftIO $
          do Right () <- writeFile "config.json" (format 2 $ json config)
               | Left err => exitError "Failed to write new config file to config.json: \{show err}."
             putStrLn "Your new configuration is:"
             printLn config
        pure config

covering
loadConfig : Octokit => Promise Config
loadConfig =
  do Right configFile <- liftIO $ readFile "config.json"
       | Left FileNotFound => createConfig
       | Left err => liftIO $ exitError "Error loading config.json: \{show err}."
     liftIO $
       case parseConfig configFile of
            Right config => pure config
            Left err => exitError err

covering
main : IO ()
main =
  do Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     [teamName] <- drop 2 <$> getArgs -- drop node & harmony.js arguments
       | []   => exitError "You must specify a team name as the first argument to harmony."
       | args => exitError "Unexpected command line arguments: \{show args}."
     _ <- octokit pat
     _ <- git
     resolve' tmp exitError $
       do config <- loadConfig
          -- liftIO $ printLn config
          pullReviewers <- listPullReviewers config.org config.repo Nothing
          teams         <- listTeams config.org
          teamMembers   <- listTeamMembers config.org teamName
          liftIO $ printLn teamMembers
          branch        <- currentBranch
          liftIO $ putStrLn "current branch: \{branch}"
          [openPr]      <- listPRsForBranch config.org config.repo branch
            | [] => liftIO $ exitError "No PR for current branch yet."
            | _  => liftIO $ exitError "Multiple PRs for the current brach. We only handle 1 PR per branch currently."
          liftIO $ printLn openPr
          _             <- addPullReviewers config.org config.repo openPr.number [] []
          pure (pullReviewers, teams, teamMembers)

