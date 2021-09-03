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
  do putStrLn "Creating a new configuration (storing in \{Config.filename})..."
     putStrLn "What GitHub org would you like to use harmony for?"
     org  <- trim <$> getLine
     putStrLn "What repository would you like to use harmony for?"
     repo <- trim <$> getLine
     putStrLn "What is the base/main branch (e.g. 'main')?"
     mainBranch <- trim <$> getLine
     updatedAt  <- cast <$> time
     do teamSlugs <- listTeams org
        let config = MkConfig {
            updatedAt
          , org
          , repo
          , mainBranch
          , teamSlugs
          }
        do Right () <- writeFile Config.filename (format 2 $ json config)
             | Left err => exitError "Failed to write new config file to \{Config.filename}: \{show err}."
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
  do configFile <- mapFst File <$> readFile Config.filename
     pure . mapFst Parse $ parseConfig configFile

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

requestBestReviewer : Config => Octokit => PullRequest -> (teamName : String) -> {default False dry: Bool} -> Promise ()
requestBestReviewer @{config} pr teamName {dry} =
  do closedReviewers <- listPullReviewers config.org config.repo (Just Closed) 30
     openReviewers   <- listPullReviewers config.org config.repo (Just Open) 40
     teams           <- listTeams config.org
     teamMembers     <- listTeamMembers config.org teamName
     -- printLn teamMembers
     user <- randomReviewer $ chooseReviewers closedReviewers openReviewers teamMembers [] pr.author
     case user of
          Nothing       => putStrLn "Could not pick a user from the given Team (perhaps the only option was the author of the pull request?)."
          (Just chosen) => do when (not dry) $
                                ignore $ addPullReviewers config.org config.repo pr.number [chosen] [teamName]
                              putStrLn "Assigned \{chosen} and team \{teamName} to the open PR for the current branch (\{pr.webURI})."
     pure ()

getManyLines : HasIO io => Fuel -> io (List String)
getManyLines = getMoreLines []
  where
    getMoreLines : (acc : List String) -> Fuel -> io (List String)
    getMoreLines acc Dry = pure (reverse acc)
    getMoreLines acc (More fuel) =
      do line <- trim <$> getLine
         -- stop collecting lines on second blank line.
         case (acc, line) of
              ("" :: rest, "") => pure (reverse rest)
              _                => getMoreLines (line :: acc) fuel

parseJiraPrefix : String -> Maybe String
parseJiraPrefix = map (pack . reverse) . guardSuccess . foldl go startOver . unpack
  where
    data Part = Start | Proj | Dash | Num | End

    startOver : (Part, List Char)
    startOver = (Start, [])

    guardSuccess : (Part, List Char) -> Maybe (List Char)
    guardSuccess (Num, y) = Just y
    guardSuccess (End, y) = Just y
    guardSuccess _ = Nothing

    go : (Part, List Char) -> Char -> (Part, List Char)
      -- start off looking for alpha characters that are a Jira Project slug.
    go (Start, cs) c   = if isAlpha c then (Proj, c :: cs) else startOver

      -- if you've found alpha characters, keep an eye out for a dash.
    go (Proj , cs) '-' = (Dash, '-' :: cs)

      -- continue parsing alpha until you find the aforementioned dash.
      -- start over if you find something else.
    go (Proj , cs) c   = if isAlpha c then (Proj, c :: cs) else startOver

      -- we expect a number after a dash or else we start over.
    go (Dash , cs) c   = if isDigit c then (Num, c :: cs) else startOver

      -- now we expect numbers until we reach the end of the prefix.
    go (Num  , cs) c   = if isDigit c then (Num, c :: cs) else (End, cs)

      -- once we are done, we just ignore the remaining characters.
    go (End  , cs) c   = (End, cs)

data IdentifiedOrCreated = Identified | Created

identifyOrCreatePR : Config => Octokit => (branch : String) -> Promise (IdentifiedOrCreated, PullRequest)
identifyOrCreatePR @{config} branch =
  do [openPr] <- listPRsForBranch config.org config.repo branch
       | [] => (Created,) <$> createPR
       | _  => reject "Multiple PRs for the current brach. We only handle 1 PR per branch currently."
     pure (Identified, openPr)
  where
    createPR : Promise PullRequest
    createPR =
      do putStrLn "Creating a new PR for the current branch (\{branch})."
         putStrLn "What branch are you merging into (ENTER for default: \{config.mainBranch})?"
         baseBranchInput <- trim <$> getLine
         let baseBranch = case strM baseBranchInput of
                               (StrCons c cs) => c `strCons` cs
                               StrNil         => config.mainBranch
         putStrLn "What would you like the title to be?"
         let titlePrefix = fromMaybe "" $ (++ " - ") <$> parseJiraPrefix branch
         putStr titlePrefix
         title <- (titlePrefix ++) . trim <$> getLine
         putStrLn "What would you like the description to be (two blank lines to finish)?"
         description <- unlines <$> getManyLines (limit 100)
         putStrLn "Creating PR..."
         GitHub.createPR config.org config.repo branch baseBranch title description

resolve'' : Promise () -> IO ()
resolve'' = resolve' pure exitError

handleArgs : Config => Git => Octokit => List String -> Promise ()
handleArgs [] =
  exitError "You must specify a subcommand as the first argument to harmony." 
handleArgs ["pr"] =
  do (Identified, pr) <- identifyOrCreatePR !currentBranch
       | _ => pure ()
     putStrLn pr.webURI
handleArgs ["assign", teamName] =
  do (_, openPr) <- identifyOrCreatePR !currentBranch
     requestBestReviewer openPr teamName
handleArgs ["assign", "--dry", teamName] =
  do (_, openPr) <- identifyOrCreatePR !currentBranch
     requestBestReviewer openPr teamName {dry=True}
handleArgs ["assign"] =
  exitError "The assign commaand expects the name of a GitHub Team as an argument."
handleArgs args =
  exitError "Unexpected command line arguments: \{show args}."

covering
main : IO ()
main =
  do Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     _ <- octokit pat
     _ <- git
     -- drop 2 for `node` and `harmony.js`
     args <- drop 2 <$> getArgs
     -- bash completion is a special case where we don't want to create the config
     -- if it doesn't exist yet so we handle it up front.
     case args of
          ["--bash-completion", curWord, prevWord] => bashCompletion curWord prevWord
          ["--bash-completion-script"] => putStrLn BashCompletion.script
          _ => resolve'' $
                 do -- create the config file before continuing if it does not exist yet
                    _ <- loadOrCreateConfig
                    -- then handle any arguments given
                    handleArgs args

