module Main

import BashCompletion
import Data.Config
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String
import FFI.Git
import FFI.GitHub
import Language.JSON
import Reviewer
import System
import System.File

import Debug.Trace

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
        do Right () <- writeFile "config.json" (format 2 $ json config)
             | Left err => exitError "Failed to write new config file to config.json: \{show err}."
           putStrLn "Your new configuration is:"
           printLn config
        pure config

data ConfigError = File FileError
                 | Parse String

Show ConfigError where
  show (File e)  = show e
  show (Parse e) = show e

covering
loadConfig : HasIO io => io (Either ConfigError Config)
loadConfig = let (>>=) = (>>=) @{Monad.Compose} in
  do configFile <- mapFst File <$> readFile "config.json"
     pure . mapFst Parse $ parseConfig configFile

covering
loadOrCreateConfig : Octokit => Promise Config
loadOrCreateConfig = 
  do Right config <- loadConfig
       | Left (File FileNotFound) => createConfig
       | Left err => exitError "Error loading config.json: \{show err}."
     pure config

covering
bashCompletion : HasIO io => (curWord : String) -> (prevWord : String) -> io ()
bashCompletion curWord prevWord = 
  do Right config <- loadConfig
       | Left _ => pure ()
     let completions = BashCompletion.opts curWord prevWord
     putStr $ unlines completions

covering
main : IO ()
main =
  do [arg1] <- drop 2 <$> getArgs -- drop node & harmony.js arguments
       | [] => exitError "You must specify a team name as the first argument to harmony."
       | ["--bash-completion", curWord, prevWord] => bashCompletion curWord prevWord
       | args => exitError "Unexpected command line arguments: \{show args}."
     when (arg1 == "--bash-completion-script") $
       do putStrLn BashCompletion.script
          exitSuccess
     let teamName = arg1
     Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     _ <- octokit pat
     _ <- git
     resolve' pure exitError $
       do config <- loadOrCreateConfig
          -- printLn config
          branch          <- currentBranch
          -- putStrLn "current branch: \{branch}"
          [openPr]        <- listPRsForBranch config.org config.repo branch
            | [] => liftIO $ exitError "No PR for current branch yet."
            | _  => liftIO $ exitError "Multiple PRs for the current brach. We only handle 1 PR per branch currently."
          -- printLn openPr
          closedReviewers <- listPullReviewers config.org config.repo (Just Closed) 30
          openReviewers   <- listPullReviewers config.org config.repo (Just Open) 40
          teams           <- listTeams config.org
          teamMembers     <- listTeamMembers config.org teamName
          -- printLn teamMembers
          user <- randomReviewer $ chooseReviewers closedReviewers openReviewers teamMembers [] openPr.author
          whenJust user $ \chosen =>
            do -- _ <- addPullReviewers config.org config.repo openPr.number [chosen] [teamName]
               putStrLn "Assigned \{chosen} and team \{teamName} to the open PR for the current branch (\{branch})."
          pure ()

