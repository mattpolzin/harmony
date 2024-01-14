module Main

import Data.Config
import Data.Promise
import Data.PullRequest
import Data.String
import Data.User

import AppVersion
import Commands
import Config
import FFI.Git
import FFI.GitHub
import Help
import JSON.Parser
import System
import System.File
import Util

import BashCompletion
import ZshCompletion

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
                 (subcommand : String)
              -> (curWord : String) 
              -> (prevWord : String) 
              -> io ()
bashCompletion subcommand curWord prevWord = 
  let completions = maybe configuredOpts pure (BashCompletion.cmdOpts subcommand curWord prevWord)
  in  putStr $ unlines !completions
  where
    configuredOpts : io (List String)
    configuredOpts =
      do Right config <- loadConfig False Nothing
           | Left _ => pure []
         pure (BashCompletion.opts subcommand curWord prevWord)

resolve'' : Promise () -> IO ()
resolve'' = resolve' pure exitError


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
handleAuthenticatedArgs @{config} ["pulls", "--json", stateFilter, pageLimit, page] =
  let args : Maybe (Maybe GitHubPRState, Fin 101, Nat) = do
    (lim, pg) <- bitraverse parseLim parsePg (pageLimit, page)
    filter <- case stateFilter of
                   "none"   => Just Nothing
                   "open"   => Just (Just Open)
                   "closed" => Just (Just Closed)
                   _        => Nothing -- <- error case
    pure (filter, lim, pg)
  in  whenJust args $ \(filter, lim, pg) => do
        pullsJsonStr <- listPullRequestsJsonStr config.org config.repo filter lim {page=pg}
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
  Commands.whoami
handleAuthenticatedArgs ["sync"] =
  Commands.sync
handleAuthenticatedArgs ["branch"] =
  Commands.branch
handleAuthenticatedArgs ("health" :: _ :: _) =
  reject "The health command does not take any arguments."
handleAuthenticatedArgs ["health"] =
  Commands.health
handleAuthenticatedArgs ("pr" :: "--draft" :: args) =
  Commands.pr {isDraft=True} args
handleAuthenticatedArgs ("pr" :: args) =
  Commands.pr {isDraft=False} args
handleAuthenticatedArgs ["reflect"] =
  Commands.reflect
handleAuthenticatedArgs ("contribute" :: args) =
  case (parseContributeArgs args) of
       Right args => Commands.contribute args
       Left err   => exitError err
handleAuthenticatedArgs ["list"] =
  reject "The list command expects the name of a GitHub Team as an argument."
handleAuthenticatedArgs @{config} ["list", teamName] =
  Commands.list teamName
handleAuthenticatedArgs @{config} ("graph" :: args) =
  case (parseGraphArgs args) of
       Right args => Commands.graph args
       Left err   => exitError err
handleAuthenticatedArgs ("assign" :: "--dry" :: assignRest) =
  Commands.assign assignRest {dry=True}
handleAuthenticatedArgs ("assign" :: assignRest) =
  Commands.assign assignRest
handleAuthenticatedArgs ["label"] =
  reject "The label command expects one or more labels as arguments."
handleAuthenticatedArgs ("label" :: label1 :: labels) =
  Commands.label (label1 :: labels)

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


-- bash and zsh completion are special cases where we don't want to create the config
-- if it doesn't exist yet so we handle it up front before loading config and then
-- handling any other input.
covering
handleArgs : Git =>
             (envGithubPAT : Maybe String)
          -> (terminalColors : Bool)
          -> (editor : Maybe String)
          -> List String 
          -> IO ()
handleArgs _ _ _ ["--bash-completion", subcommand, curWord, prevWord] = bashCompletion subcommand curWord prevWord
handleArgs _ _ _ ["--bash-completion-script"] = putStrLn BashCompletion.script
handleArgs _ _ _ ["--zsh-completion-script"] = putStrLn ZshCompletion.script
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
       putStrLn (help terminalColors)
       exitSuccess
     when (args == ["version"] || args == ["--version"]) $ do
       printVersion
       exitSuccess
     envPAT <- getEnv "GITHUB_PAT"
     _ <- git
     handleArgs envPAT terminalColors editor args

