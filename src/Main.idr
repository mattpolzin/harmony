module Main

import AppVersion
import BashCompletion
import Config as Cfg
import Data.Config
import Data.Date
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String
import Data.String.Extra
import Data.User
import FFI.Concurrency
import FFI.Git
import FFI.GitHub
import Help
import Language.JSON
import Language.JSON.Accessors
import PullRequest as PR
import Reviewer
import System
import System.File
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import User

%default total

exitError : HasIO io => 
            String 
         -> io a
exitError err =
  do stderrColors <- isTTY stderr
     if stderrColors
          then ignore $ fPutStrLn stderr . renderString . layoutPretty defaultLayoutOptions . annotate (color Red) . pretty $ trim err
          else ignore $ fPutStrLn stderr err
     exitFailure

covering
bashCompletion : HasIO io => 
                 (curWord : String) 
              -> (prevWord : String) 
              -> io ()
bashCompletion curWord prevWord = 
  let completions = maybe configuredOpts pure (cmdOpts curWord prevWord)
  in  putStr $ unlines !completions
  where
    configuredOpts : io (List String)
    configuredOpts =
      do Right config <- loadConfig False
           | Left _ => pure []
         pure (BashCompletion.opts curWord prevWord)

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

listTeam : Config => Octokit =>
           (team : String) 
        -> Promise ()
listTeam @{config} team =
  do teamMemberLogins <- sort <$> listTeamMembers config.org team
     teamMembersJson <- promiseAll =<< traverse forkedUser teamMemberLogins
     teamMembers <- traverse (either . parseUser) teamMembersJson
     liftIO . putDoc . vsep $ putNameLn <$> teamMembers
  where
    forkedUser : (login : String) -> Promise Future
    forkedUser = fork . ("user --json " ++)

    putNameLn : User -> Doc AnsiStyle
    putNameLn user =
      hsep [(fillBreak 15 . annotate italic $ pretty user.login), "-", (pretty user.name)]

graphTeam : Config => Octokit =>
            (team : String) 
         -> Promise ()
graphTeam @{config} team =
  do teamMemberLogins <- listTeamMembers config.org team
     (openReviewers, closedReviewers) <- listReviewers 100 {pageBreaks=4}
     liftIO . putDoc . maybeDecorated $ reviewsGraph closedReviewers openReviewers teamMemberLogins
  where
    maybeDecorated : Doc AnsiStyle -> Doc AnsiStyle
    maybeDecorated = if config.colors then id else unAnnotate

contribute : Config => Octokit =>
             Nat
          -> Promise ()
contribute @{config} skip =
  do openPrs <- listPullRequests config.org config.repo (Just Open) 100
     myLogin <- login <$> getSelf
     let filtered = filter (not . isAuthor myLogin) openPrs
     let parted = partition (isRequestedReviewer myLogin) filtered
     let (mine, theirs) = (mapHom $ sortBy (compare `on` .createdAt)) parted
     let url = map (.webURI) . head' . drop skip $ mine ++ theirs
     printResult url
  where
    printResult : Maybe String -> Promise ()
    printResult Nothing    = reject "No open PRs to review!"
    printResult (Just url) = putStrLn url

handleConfiguredArgs : Config => Git => Octokit => 
                       List String 
                    -> Promise ()
handleConfiguredArgs @{config} [] =
  putStrLn $ help config.colors

-- internal-use commands for forking process:
handleConfiguredArgs @{config} ["reviews", "--json", prNumber] =
  whenJust (parsePositive prNumber) $ \pr => do
    reviewsJsonStr <- listPullReviewsJsonStr config.org config.repo pr
    putStr reviewsJsonStr
handleConfiguredArgs @{config} ["pulls", "--json", pageLimit, page] =
  let args : Maybe (Fin 101, Nat) = bitraverse parseLim parsePg (pageLimit, page)
  in  whenJust args $ \(lim, pg) => do
        pullsJsonStr <- listPullRequestsJsonStr config.org config.repo Nothing lim {page=pg}
        putStr pullsJsonStr
  where
    parseLim : String -> Maybe (Fin 101)
    parseLim = (\x => natToFin x 101) <=< parsePositive

    parsePg  : String -> Maybe Nat
    parsePg = parsePositive
handleConfiguredArgs @{config} ["user", "--json", username] =
  print $ json !(getUser username)

-- user-facing commands:
handleConfiguredArgs ["sync"] =
  ignore $ syncConfig True
handleConfiguredArgs ["pr"] =
  do (Identified, pr) <- identifyOrCreatePR !currentBranch
       | _ => pure ()
     putStrLn pr.webURI
handleConfiguredArgs ["reflect"] =
  reflectOnSelf
handleConfiguredArgs ["config", prop] =
  do value <- getConfig prop
     putStrLn value
handleConfiguredArgs ["config", prop, value] =
  ignore $ setConfig prop value
handleConfiguredArgs ["contribute"] =
  contribute 0
handleConfiguredArgs ["contribute", skipArg] =
  case unpack skipArg of
       ('-' :: skip) => do 
         let (Just num) : Maybe Nat = map cast . parsePositive $ pack skip
           | Nothing => exitError "contribute's argument must be -<num> where <num> is an integer."
         contribute num
       _             => exitError "contribute's argument must be -<num> where <num> is an integer."
handleConfiguredArgs ["list"] =
  reject "The list command expects the name of a GitHub Team as an argument."
handleConfiguredArgs @{config} ["list", teamName] =
  listTeam teamName
handleConfiguredArgs ["graph"] =
  reject "The graph command expects the name of a GitHub Team as an argument."
handleConfiguredArgs @{config} ["graph", teamName] =
  graphTeam teamName
handleConfiguredArgs ["assign"] =
  reject "The assign commaand expects one or more names of GitHub Teams or Users as arguments."
handleConfiguredArgs ["assign", "--dry"] =
  reject "The assign commaand expects one or more names of GitHub Teams or Users as arguments."
handleConfiguredArgs ("assign" :: "--dry" :: assign1 :: assignRest) =
  assign (assign1 :: assignRest) {dry=True}
handleConfiguredArgs ("assign" :: assign1 :: assignRest) =
  assign (assign1 :: assignRest)

-- error case:
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
  resolve'' $
    do -- create the config file before continuing if it does not exist yet
       _ <- syncIfOld =<< loadOrCreateConfig terminalColors
       -- then handle any arguments given
       handleConfiguredArgs args

shouldUseColors : HasIO io => io Bool
shouldUseColors = do
  tty <- isTTY stdout
  noColors <- getEnv "NO_COLOR"
  pure (isNothing noColors && tty)

covering
main : IO ()
main =
  do terminalColors <- shouldUseColors
     -- drop 2 for `node` and `harmony.js`
     args <- drop 2 <$> getArgs
     -- short circuit for help
     when (args == ["help"] || args == ["--help"]) $ do
       putStrLn $ help terminalColors
       exitSuccess
     when (args == ["version"] || args == ["--version"]) $ do
       printVersion
       exitSuccess
     -- otherwise get a GitHub Personal Access Token and continue.
     Just pat <- getEnv "GITHUB_PAT"
       | Nothing => exitError "GITHUB_PAT environment variable must be set to a personal access token."
     _ <- octokit pat
     _ <- git
     handleArgs terminalColors args

