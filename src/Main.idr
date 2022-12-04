module Main

import AppVersion
import BashCompletion
import Config as Cfg
import Data.Config
import Data.Date
import Data.List
import Data.Promise
import Data.PullRequest
import Data.SortedMap
import Data.String
import Data.String.Extra
import Data.User
import FFI.Concurrency
import FFI.Git
import FFI.GitHub
import Graph
import Help
import Label
import Language.JSON
import Language.JSON.Accessors
import PullRequest as PR
import Reviewer
import System
import System.File
import User
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

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
      do Right config <- loadConfig False Nothing
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

label : Config => Git => Octokit =>
        (labels : List String)
     -> Promise ()
label labels =
  do (_, openPr) <- identifyOrCreatePR !currentBranch
     allLabels <- addLabels openPr labels
     renderIO $ vsep
       [ "Added" <++> putLabels labels <+> " to PR."
       , pretty "All labels for PR of \{openPr.headRef}:" <++> putLabels allLabels <+> "."
       ]
  where
    putLabel : String -> Doc AnsiStyle
    putLabel = enclose "\"" "\"" . annotate (color Green) . pretty

    putLabels : List String -> Doc AnsiStyle
    putLabels = hcat . intersperse (pretty ", ") . map putLabel

listTeam : Config => Octokit =>
           (team : String) 
        -> Promise ()
listTeam @{config} team =
  do teamMemberLogins <- sort <$> listTeamMembers config.org team
     teamMembersJson <- promiseAll =<< traverse forkedUser teamMemberLogins
     teamMembers <- traverse (either . parseUser) teamMembersJson
     renderIO . vsep $ putNameLn <$> teamMembers
  where
    forkedUser : (login : String) -> Promise Future
    forkedUser = fork . ("user --json " ++)

    putNameLn : User -> Doc AnsiStyle
    putNameLn user =
      hsep [(fillBreak 15 . annotate italic $ pretty user.login), "-", (pretty user.name)]

data GraphArg : Type where
  TeamName : String -> GraphArg
  IncludeCompletedReviews : GraphArg

teamNameArg : GraphArg -> Maybe String
teamNameArg (TeamName n) = Just n
teamNameArg _ = Nothing

graphTeam : Config => Octokit =>
            List GraphArg
         -> Promise ()
graphTeam @{config} args = do
  let includeCompletedReviews = find (\case IncludeCompletedReviews => True; _ => False) args
  let Just teamName = head' $ mapMaybe teamNameArg args
    | Nothing => reject "The graph command expects the name of a GitHub Team as an argument."
  teamMemberLogins <- listTeamMembers config.org teamName
  prs <- listPartitionedPRs 100 {pageBreaks=4}
  let (openReviewers, closedReviewers) = prs.allReviewers
  completedReviews <- 
    case (isJust includeCompletedReviews) of
         True  => countReviewsByEachUser (combined prs)
         False => pure empty
  renderIO $ reviewsGraph closedReviewers openReviewers teamMemberLogins (Just completedReviews)

(<||>) : Alternative t => (a -> t b) -> (a -> t b) -> a -> t b
(<||>) f g x = f x <|> g x

infix 2 <||>

parseGraphArgs : List String -> Either String (List GraphArg)
parseGraphArgs [] = Right []
parseGraphArgs (x :: y :: z :: xs) =
  Left "graph accepts at most one team name and the --completed flag."
parseGraphArgs args =
  case (traverse (parseCompletedFlag <||> parseTeamArg) args) of
       Just args => Right args
       Nothing   =>
         Left "The graph command expects the name of a GitHub Team and optionally --completed as arguments."
  where
    parseCompletedFlag : String -> Maybe GraphArg
    parseCompletedFlag "-c" = Just IncludeCompletedReviews
    parseCompletedFlag "--completed" = Just IncludeCompletedReviews
    parseCompletedFlag _ = Nothing

    parseTeamArg : String -> Maybe GraphArg
    parseTeamArg str = Just (TeamName str)

data ContributeArg = Checkout | Skip Nat

skipArg : ContributeArg -> Maybe Nat
skipArg (Skip n) = Just n
skipArg _ = Nothing

contribute : Config => Git => Octokit =>
             (args : List ContributeArg)
          -> Promise ()
contribute @{config} args =
  do openPrs <- listPullRequests config.org config.repo (Just Open) 100
     myLogin <- login <$> getSelf
     let skip = fromMaybe 0 (head' $ mapMaybe skipArg args)
     let checkout = find (\case Checkout => True; _ => False) args
     let filtered = filter (not . isAuthor myLogin) openPrs
     let parted = partition (isRequestedReviewer myLogin) filtered
     let (mine, theirs) = (mapHom $ sortBy (compare `on` .createdAt)) parted
     let pr = head' . drop skip $ mine ++ theirs
     case pr of
          Nothing => reject "No open PRs to review!"
          Just pr => do
            whenJust (checkout $> pr.headRef) $ \branch => do
              checkoutBranch branch
            putStrLn  pr.webURI

parseContributeArgs : List String -> Either String (List ContributeArg)
parseContributeArgs [] = Right []
parseContributeArgs (_ :: _ :: _ :: _) =
  Left "contribute's arguments must be either -<num> to skip num PRs or --checkout (-c) to checkout the branch needing review."
parseContributeArgs args =
  case (traverse (parseSkipArg <||> parseCheckoutFlag) args) of
       Just args => Right args
       Nothing   =>
         Left "contribute's arguments must be either -<num> to skip num PRs or --checkout (-c) to checkout the branch needing review."
  where
    parseCheckoutFlag : String -> Maybe ContributeArg
    parseCheckoutFlag "-c" = Just Checkout
    parseCheckoutFlag "--checkout" = Just Checkout
    parseCheckoutFlag _ = Nothing

    parseSkipArg : String -> Maybe ContributeArg
    parseSkipArg skipArg =
      case unpack skipArg of
           ('-' :: skip) => map (Skip . cast) . parsePositive $ pack skip
           _             => Nothing

printCurrentBranchURI : Config => Git => Promise ()
printCurrentBranchURI @{config} = do
  branch <- currentBranch
  let org = config.org
  let repo = config.repo
  let uri = "https://github.com/\{org}/\{repo}/tree/\{branch}"
  putStrLn uri

||| Handle commands that require both configuration and
||| authentication.
handleAuthenticatedArgs : Config => Git => Octokit => 
                          List String 
                       -> Promise ()

-- internal-use commands for forking process:
handleAuthenticatedArgs @{config} ["reviews", "--json", prNumber] =
  whenJust (parsePositive prNumber) $ \pr => do
    reviewsJsonStr <- listPullReviewsJsonStr config.org config.repo pr
    putStr reviewsJsonStr
handleAuthenticatedArgs @{config} ["pulls", "--json", pageLimit, page] =
  let args : Maybe (Fin 101, Nat) = bitraverse parseLim parsePg (pageLimit, page)
  in  whenJust args $ \(lim, pg) => do
        pullsJsonStr <- listPullRequestsJsonStr config.org config.repo Nothing lim {page=pg}
        putStr pullsJsonStr
  where
    parseLim : String -> Maybe (Fin 101)
    parseLim = (\x => natToFin x 101) <=< parsePositive

    parsePg  : String -> Maybe Nat
    parsePg = parsePositive
handleAuthenticatedArgs @{config} ["user", "--json", username] =
  print $ json !(getUser username)

-- user-facing commands:
handleAuthenticatedArgs ["whoami"] =
  printInfoOnSelf
handleAuthenticatedArgs ["sync"] =
  ignore $ syncConfig True
handleAuthenticatedArgs ["branch"] =
  printCurrentBranchURI
handleAuthenticatedArgs ["pr"] =
  do (Identified, pr) <- identifyOrCreatePR !currentBranch
       | _ => pure ()
     putStrLn pr.webURI
handleAuthenticatedArgs ["reflect"] =
  reflectOnSelf
handleAuthenticatedArgs ("contribute" :: args) =
  case (parseContributeArgs args) of
       Right args => contribute args
       Left err   => exitError err
handleAuthenticatedArgs ["list"] =
  reject "The list command expects the name of a GitHub Team as an argument."
handleAuthenticatedArgs @{config} ["list", teamName] =
  listTeam teamName
handleAuthenticatedArgs @{config} ("graph" :: args) =
  case (parseGraphArgs args) of
       Right args => graphTeam args
       Left err   => exitError err
handleAuthenticatedArgs ["assign"] =
  reject "The assign command expects one or more names of GitHub Teams or Users as arguments."
handleAuthenticatedArgs ["assign", "--dry"] =
  reject "The assign command expects one or more names of GitHub Teams or Users as arguments."
handleAuthenticatedArgs ("assign" :: "--dry" :: assign1 :: assignRest) =
  assign (assign1 :: assignRest) {dry=True}
handleAuthenticatedArgs ("assign" :: assign1 :: assignRest) =
  assign (assign1 :: assignRest)
handleAuthenticatedArgs ["label"] =
  reject "The label command expects one or more labels as arguments."
handleAuthenticatedArgs ("label" :: label1 :: labels) =
  label (label1 :: labels)

-- error case:
handleAuthenticatedArgs args =
  reject "Unexpected command line arguments: \{show args}."


||| Handle commands that only require configuration, or else
||| enforce authentication and handle commands that require auth.
handleConfiguredArgs : Config => Git =>
                       (envGithubPAT : Maybe String)
                    -> List String
                    -> Promise ()
handleConfiguredArgs _ ["config"] =
  reject $ "The config command expects one or two arguments. "
        ++ "Specify a property to read out or a property and a value to set it to."
        ++ "\n\n"
        ++ settablePropsWithHelp
handleConfiguredArgs _ ["config", prop] =
  do value <- getConfig prop
     putStrLn value
handleConfiguredArgs _ ["config", prop, value] =
  ignore $ setConfig prop value

handleConfiguredArgs @{config} envPAT args = do
   -- Personal access token either comes from ENV or from config.
   Just pat <- pure $ envPAT <|> expose <$> config.githubPAT
     | Nothing => reject $ "Either the GITHUB_PAT environment variable or githubPAT config "
                        ++ "property must be set to a personal access token."
   _ <- liftIO $ octokit pat

   config' <- syncIfOld config

   -- then handle any arguments given
   handleAuthenticatedArgs @{config'} args


-- bash completion is a special case where we don't want to create the config
-- if it doesn't exist yet so we handle it up front before loading config and then
-- handling any other input.
covering
handleArgs : Git =>
             (envGithubPAT : Maybe String)
          -> (terminalColors : Bool)
          -> (editor : Maybe String)
          -> List String 
          -> IO ()
handleArgs _ _ _ ["--bash-completion", curWord, prevWord] = bashCompletion curWord prevWord
handleArgs _ _ _ ["--bash-completion-script"] = putStrLn BashCompletion.script
handleArgs envPAT terminalColors editor args = 
  resolve'' $
    do -- create the config file before continuing if it does not exist yet
       config <- loadOrCreateConfig envPAT terminalColors editor

       handleConfiguredArgs envPAT args

shouldUseColors : HasIO io => io Bool
shouldUseColors = do
  tty <- isTTY stdout
  noColors <- getEnv "NO_COLOR"
  pure (isNothing noColors && tty)

covering
main : IO ()
main =
  do terminalColors <- shouldUseColors
     editor <- getEnv "EDITOR"
     -- drop 1 for `harmony.js`
     args <- drop 1 <$> getArgs
     -- short circuit for help
     when (args == [] || args == ["help"] || args == ["--help"]) $ do
       putStrLn $ help terminalColors
       exitSuccess
     when (args == ["version"] || args == ["--version"]) $ do
       printVersion
       exitSuccess
     envPAT <- getEnv "GITHUB_PAT"
     _ <- git
     handleArgs envPAT terminalColors editor args

