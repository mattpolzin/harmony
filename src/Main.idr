module Main

import BashCompletion
import Data.Config
import Data.Either
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String
import Data.String.Extra
import FFI.Git
import FFI.GitHub
import Help
import Language.JSON
import PullRequest as PR
import System
import System.File

import Debug.Trace

%default total

exitError : HasIO io => String -> io a
exitError err =
  do putStrLn err
     exitFailure

writeConfig : Config -> Promise ()
writeConfig config =
  do res <- writeFile config.filepath (format 2 $ json config)
     case res of
          Right () => pure ()
          Left err => reject "Failed to write updated config file to \{config.filepath}: \{show err}."

syncConfig : Config => Octokit => (echo : Bool) -> Promise Config
syncConfig @{config} echo =
 do teamSlugs  <- listTeams config.org
    orgMembers <- listOrgMembers config.org
    updatedAt  <- cast {to=Timestamp} <$> time
    let config' = { updatedAt := updatedAt, teamSlugs := teamSlugs, orgMembers := orgMembers } config
    writeConfig config'
    when echo $
      do putStrLn "Your updated configuration is:"
         printLn config
    pure config'

syncIfOld : Octokit => Config -> Promise Config
syncIfOld config =
  if config.updatedAt < !oneDayAgo
     then do putStrLn "Syncing config file..."
             syncConfig False
     else pure config
  where
    oneDayAgo : HasIO io => io Timestamp
    oneDayAgo =
      do let oneDay = 86_400
         now <- time
         pure $ cast (now - oneDay)

createConfig : Octokit => Promise Config
createConfig = 
  do putStrLn "Creating a new configuration (storing in \{Config.filename})..."
     putStrLn "What GitHub org would you like to use harmony for?"
     org  <- trim <$> getLine
     putStrLn "What repository would you like to use harmony for?"
     repo <- trim <$> getLine
     putStrLn "What is the base/main branch (e.g. 'main')?"
     mainBranch <- trim <$> getLine
     updatedAt  <- cast <$> time
     do teamSlugs  <- listTeams org
        orgMembers <- listOrgMembers org
        let config = MkConfig {
            updatedAt
          , org
          , repo
          , mainBranch
          , teamSlugs
          , orgMembers
          , filepath = "."
          }
        writeConfig config
        putStrLn "Your new configuration is:"
        printLn config
        pure config

data ConfigError = File FileError
                 | Parse String

Show ConfigError where
  show (File e)  = show e
  show (Parse e) = show e

findConfig : HasIO io => (startDir : String) -> Fuel -> io (Maybe String)
findConfig startDir Dry = pure Nothing
findConfig startDir (More fuel) = 
  let location = "\{startDir}/\{Config.filename}"
  in if !(exists location)
       then pure (Just location)
       else findConfig "\{startDir}/.." fuel

covering
loadConfig : HasIO io => io (Either ConfigError Config)
loadConfig = let (>>=) = (>>=) @{Monad.Compose} in
  do location   <- mapFst File . maybeToEither FileNotFound <$>
                     findConfig "." (limit 10)
     configFile <- mapFst File <$> 
                     readFile location
     pure . mapFst Parse $ parseConfig location configFile

covering
loadOrCreateConfig : Octokit => Promise Config
loadOrCreateConfig = 
  do Right config <- loadConfig
       | Left (File FileNotFound) => createConfig
       | Left err => exitError "Error loading \{Config.filename}: \{show err}."
     pure config

covering
bashCompletion : HasIO io => (curWord : String) -> (prevWord : String) -> io ()
bashCompletion curWord prevWord = 
  do Right config <- loadConfig
       | Left _ => pure ()
     let completions = BashCompletion.opts curWord prevWord
     putStr $ unlines completions

resolve'' : Promise () -> IO ()
resolve'' = resolve' pure exitError

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

handleConfiguredArgs : Config => Git => Octokit => List String -> Promise ()
handleConfiguredArgs [] =
  reject "You must specify a subcommand as the first argument to harmony." 
handleConfiguredArgs ["help"] =
  putStrLn help
handleConfiguredArgs ["--help"] =
  putStrLn help
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
handleArgs : Git => Octokit => List String -> IO ()
handleArgs ["--bash-completion", curWord, prevWord] = bashCompletion curWord prevWord
handleArgs ["--bash-completion-script"] = putStrLn BashCompletion.script
handleArgs args = resolve'' $
  do -- create the config file before continuing if it does not exist yet
     _ <- syncIfOld =<< loadOrCreateConfig
     -- then handle any arguments given
     handleConfiguredArgs args

covering
main : IO ()
main =
  do Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     _ <- octokit pat
     _ <- git
     -- drop 2 for `node` and `harmony.js`
     handleArgs $ drop 2 !getArgs

