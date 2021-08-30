module Main

import Data.Either
import Data.List
import Data.Promise
import Data.String
import Data.Vect
import Language.JSON
import Language.JSON.Accessors
import System
import System.File
import Data.PullRequest
import FFI.Git
import FFI.GitHub

%default total

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

