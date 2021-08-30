module Main

import Data.Config
import Data.List
import Data.Promise
import Data.String
import Language.JSON
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
     resolve' pure exitError $
       do config <- loadConfig
          -- liftIO $ printLn config
          pullReviewers <- listPullReviewers config.org config.repo Nothing
          teams         <- listTeams config.org
          teamMembers   <- listTeamMembers config.org teamName
          -- liftIO $ printLn teamMembers
          branch        <- currentBranch
          -- liftIO $ putStrLn "current branch: \{branch}"
          [openPr]      <- listPRsForBranch config.org config.repo branch
            | [] => liftIO $ exitError "No PR for current branch yet."
            | _  => liftIO $ exitError "Multiple PRs for the current brach. We only handle 1 PR per branch currently."
          -- liftIO $ printLn openPr
          let user = ""
          _             <- addPullReviewers config.org config.repo openPr.number [user] []
          when (user /= "") $
            liftIO $ putStrLn "Assigned \{user} to the open PR for the current branch (\{branch})."
          pure ()

