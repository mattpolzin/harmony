module Main

import BashCompletion
import Config as Cfg
import Control.ANSI
import Data.Config
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String
import Data.String.Extra
import FFI.Git
import FFI.GitHub
import Help
import PullRequest as PR
import System
import System.File.Meta
import System.File.Virtual

%default total

exitError : HasIO io => 
            (terminalColors : Bool)
         -> String 
         -> io a
exitError terminalColors err =
  do if terminalColors then printLn $ colored Red err else putStrLn err
     exitFailure

covering
bashCompletion : HasIO io => 
                 (curWord : String) 
              -> (prevWord : String) 
              -> io ()
bashCompletion curWord prevWord = 
  do Right config <- loadConfig False
       | Left _ => pure ()
     let completions = BashCompletion.opts curWord prevWord
     putStr $ unlines completions

resolve'' : (terminalColors : Bool) -> Promise () -> IO ()
resolve'' terminalColors = resolve' pure (exitError terminalColors)

assign : Config => Git => Octokit => 
         (assignArgs : List String) 
      -> {default False dry : Bool} 
      -> Promise ()
assign args {dry} =
  do (_, openPr) <- identifyOrCreatePR !currentBranch
     let (forcedReviewers, teamNames) = partitionedArgs
     requestReviewers openPr teamNames forcedReviewers {dry}
  where
    -- partition args into user logins and team slugs
    partitionedArgs : (List String, List String)
    partitionedArgs = 
      let part = partition (isPrefixOf "+") args
      in  mapFst (map $ drop 1) part

handleConfiguredArgs : Config => Git => Octokit => 
                       List String 
                    -> Promise ()
handleConfiguredArgs [] =
  reject "You must specify a subcommand as the first argument to harmony." 
handleConfiguredArgs @{config} ["help"] =
  putStrLn $ help config.colors
handleConfiguredArgs @{config} ["--help"] =
  putStrLn $ help config.colors
handleConfiguredArgs ["sync"] =
  ignore $ syncConfig True
handleConfiguredArgs ["pr"] =
  do (Identified, pr) <- identifyOrCreatePR !currentBranch
       | _ => pure ()
     putStrLn pr.webURI
handleConfiguredArgs ["list"] =
  reject "The list command expects the name of a GitHub Team as an argument."
handleConfiguredArgs @{config} ["list", teamName] =
  do teamMembers <- listTeamMembers config.org teamName
     traverse_ putStrLn teamMembers
handleConfiguredArgs ["assign"] =
  reject "The assign commaand expects one or more names of GitHub Teams or Users as arguments."
handleConfiguredArgs ["assign", "--dry"] =
  reject "The assign commaand expects one or more names of GitHub Teams or Users as arguments."
handleConfiguredArgs ("assign" :: "--dry" :: assign1 :: assignRest) =
  assign (assign1 :: assignRest) {dry=True}
handleConfiguredArgs ("assign" :: assign1 :: assignRest) =
  assign (assign1 :: assignRest)
handleConfiguredArgs args =
  reject "Unexpected command line arguments: \{show args}."

-- bash completion is a special case where we don't want to create the config
-- if it doesn't exist yet so we handle it up front before loading config and then
-- handling any other input.
covering
handleArgs : Git => Octokit => 
             (terminalColors : Bool)
          -> List String 
          -> IO ()
handleArgs _ ["--bash-completion", curWord, prevWord] = bashCompletion curWord prevWord
handleArgs _ ["--bash-completion-script"] = putStrLn BashCompletion.script
handleArgs terminalColors args = 
  resolve'' terminalColors $
    do -- create the config file before continuing if it does not exist yet
       _ <- syncIfOld =<< loadOrCreateConfig terminalColors
       -- then handle any arguments given
       handleConfiguredArgs args

covering
main : IO ()
main =
  do terminalColors <- isTTY stdout
     Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError terminalColors "GITHUB_PAT environment variable must be set to a personal access token."
     _ <- octokit pat
     _ <- git
     -- drop 2 for `node` and `harmony.js`
     handleArgs terminalColors $ drop 2 !getArgs

