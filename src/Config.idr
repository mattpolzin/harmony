module Config

import Data.Config
import Data.Either
import Data.List
import Data.List.PrefixSuffix
import Data.List1
import Data.Promise
import Data.String
import Decidable.Equality
import FFI.Git
import FFI.GitHub
import JSON.Parser
import System
import System.File
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

writeConfig : Config -> Promise Config
writeConfig config =
  do res <- writeFile config.filepath (show $ json config)
     case res of
          Right () => pure config
          Left err => reject "Failed to write updated config file to \{config.filepath}: \{show err}."

export
syncConfig : Config => Octokit => (echo : Bool) -> Promise Config
syncConfig @{config} echo =
 do teamSlugs  <- listTeams config.org
    labelNames <- listRepoLabels config.org config.repo
    orgMembers <- listOrgMembers config.org
    updatedAt  <- cast {to=Data.Config.Timestamp} <$> time
    let config' = { updatedAt  := updatedAt
                  , teamSlugs  := teamSlugs
                  , repoLabels := labelNames
                  , orgMembers := orgMembers
                  } config
    ignore $ writeConfig config'
    when echo $
      do putStrLn "Your updated configuration is:"
         printLn config'
    pure config'

export
syncIfOld : Octokit => Config -> Promise Config
syncIfOld config =
  if config.updatedAt < !oneDayAgo
     then do -- putStrLn "Syncing config file..."
             syncConfig False
     else pure config
  where
    oneDayAgo : HasIO io => io Data.Config.Timestamp
    oneDayAgo =
      do let oneDay = 86_400
         now <- time
         pure $ cast (now - oneDay)

||| Add the given PR numbers to the list of ignored PRs in the
||| config file. This results in a write to the config file and
||| also returns the updated config.
export
addIgnoredPRs : Config -> List Integer -> Promise Config
addIgnoredPRs config is =
  writeConfig $
    { ignoredPRs := (nub $ config.ignoredPRs ++ is) } config

||| Determine if any configuration settings are inconsistent with each other or result in
||| behavior that is likely undesirable.
checkConfigConsistency : Config -> Either (Doc AnsiStyle) ()
checkConfigConsistency config = do
  checkRequestSettings config
  -- other checks...
  where
    checkRequestSettings : Config -> Either (Doc AnsiStyle) ()
    checkRequestSettings config =
      if not (config.requestTeams || config.requestUsers)
         then Left $ (annotate (color Yellow) . hsep $ [
                "`requestUsers` and `requestTeams` are both False."
              , "This means `harmony request` commands will only ever request reviews from users that are specified with the `+<userlogin>` syntax."
              , "More commonly, you want Harmony to at least request review for either a team or a user from a team when you say `harmony request teamname`;"
              , "It's suggested to either `harmony config requestUsers true` or `harmony config requestTeams true` (or both)."
              ]) <+> hardline
         else Right ()

record GitRemote where
  constructor Remote
  org, repo : String

dropPrefix' : (prefx : String) -> String -> Maybe String
dropPrefix' prefx = map pack . drop' . unpack
  where
    drop' : List Char -> Maybe (List Char)
    drop' xs with (dropPrefix (unpack prefx) xs)
      drop' xs | (Yes (suffix ** _)) = Just suffix
      drop' xs | (No contra)         = Nothing

parseGitHubURI : String -> Maybe GitRemote
parseGitHubURI str = parseHTTPS str <|> parseSSH str
  where
    parseSuffix : String -> Maybe GitRemote
    parseSuffix suffix =
      do let (orgAndRepo, _) = break (== '.') suffix
         (org ::: repo :: []) <- pure $ split (== '/') orgAndRepo
           | _ => Nothing
         pure $ Remote org repo

    parseHTTPS : String -> Maybe GitRemote
    parseHTTPS = dropPrefix' "https://github/com/" >=> parseSuffix

    parseSSH : String -> Maybe GitRemote
    parseSSH = dropPrefix' "git@github.com:" >=> parseSuffix

parseBool : String -> Maybe Bool
parseBool x with (toLower x)
  _ | "yes"   = Just True
  _ | "true"  = Just True
  _ | "no"    = Just False
  _ | "false" = Just False
  _ | _ = Nothing

update : Functor f => (String -> f a) -> (a -> b -> b) -> b -> String -> f b
update f g c = map (flip g c) . f

propSetter : SettableProp n h -> (Config -> String -> Maybe Config)
propSetter RequestTeams     = update parseBool (\b => { requestTeams := b })
propSetter RequestUsers     = update parseBool (\b => { requestUsers := b })
propSetter CommentOnRequest = update parseBool (\b => { commentOnRequest := b })
propSetter DefaultRemote    = update Just (\s => { defaultRemote := s })
propSetter GithubPAT        = update Just (\s => { githubPAT := Just $ hide s })

||| Attempt to set a property and value given String representations.
||| After setting, write the config and return the updated result.
export
setConfig : Config =>
            (prop : String)
         -> (value : String)
         -> Promise Config
setConfig @{config} prop value with (settablePropNamed prop)
  _ | Nothing = reject "\{prop} cannot be set via `config` command."
  _ | Just (Evidence _ p) with ((propSetter p) config value)
    _ | Nothing = reject "\{value} is not a valid value for \{prop}."
    _ | Just config' = do either (renderIO @{config'}) pure (checkConfigConsistency config')
                          writeConfig config'

propGetter : SettableProp n h -> (Config -> String)
propGetter RequestTeams     = show . requestTeams
propGetter RequestUsers     = show . requestUsers
propGetter CommentOnRequest = show . commentOnRequest
propGetter DefaultRemote    = show . defaultRemote
propGetter GithubPAT        = maybe "Not set (will use $GITHUB_PAT environment variable)" show . githubPAT

export
getConfig : Config =>
            (prop : String)
         -> Promise String
getConfig @{config} prop with (settablePropNamed prop)
  getConfig @{config} prop | Nothing = reject "\{prop} cannot get read via `config` command."
  getConfig @{config} prop | (Just (Evidence _ p)) = pure $ (propGetter p) config

export
settablePropsWithHelp : Config => String
settablePropsWithHelp = renderString . vsep $ help <$> settablePropNamesAndHelp
  where
    help : (String, String) -> Doc AnsiStyle
    help (n, h) = (annotate (color Green) $ pretty n) <+> pretty ": \{replicate (longestSettablePropName `minus` (length n)) ' ' ++ h}"

||| Look for "origin" in a list of remote names or else
||| fallback to the first name.
preferOriginRemote : List String -> String
preferOriginRemote names =
  case find (== "origin") names of
       Just n  => n
       Nothing => fromMaybe "origin" (head' names)

createConfig : Git =>
               (envGithubPAT : Maybe String)
            -> (terminalColors : Bool)
            -> (editor : Maybe String)
            -> Promise Config
createConfig envGithubPAT terminalColors editor = do
  putStrLn "Creating a new configuration (storing in \{Config.filename})..."
  putStrLn ""

  let defaultPATString = enterForDefaultStr "unset"
  putStr $ unlines [ "Harmony uses a GitHub Personal Access Token (PAT) to communicate with GitHub."
                   , "You can set this via the $GITHUB_PAT environment variable or a config property."
                   , "If you don't set in your config now, you can set later with `harmony config githubPAT abcdefg`."
                   , "The ENV var will always take precedence over the config property."
                   , ""
                   , "What PAT would you like to set in the config file\{defaultPATString}?"
                   ]
  configPAT <- (\case "" => Nothing; s => Just s) . trim <$> getLine

  -- Personal access token either comes from the ENV or the config property
  Just pat <- pure $ envGithubPAT <|> configPAT
    | _ => reject $ "Either the GITHUB_PAT environment variable or githubPAT config "
                 ++ "property must be set to a personal access token."

  remoteGuess <- preferOriginRemote <$> listRemotes
  defaultOrgAndRepo <- (parseGitHubURI <$> remoteURI remoteGuess) <|> pure Nothing

  let orgDefaultStr = defaultStr (.org) defaultOrgAndRepo
  putStrLn "What GitHub org would you like to use harmony for\{orgDefaultStr}?"
  org  <- orIfEmpty (org defaultOrgAndRepo) . trim <$> getLine

  let repoDefaultStr = defaultStr (.repo) defaultOrgAndRepo
  putStrLn "What repository would you like to use harmony for\{repoDefaultStr}?"
  repo <- orIfEmpty (repo defaultOrgAndRepo) . trim <$> getLine

  let remoteDefaultStr = enterForDefaultStr remoteGuess
  putStrLn "What GitHub remote repo would you like to use harmony for\{remoteDefaultStr}?"
  defaultRemote <- orIfEmpty (Just remoteGuess) . trim <$> getLine
  
  commentOnRequest <-
    yesNoPrompt "Would you like harmony to comment when it requests reviewers?"

  requestTeams <-
    yesNoPrompt "Would you like harmony to request reviews from teams when it requests reviewers?"

  requestUsers <-
    yesNoPrompt "Would you like harmony to request reviews from individual users when it requests a teams review?"

  _ <- liftIO $ octokit pat
  putStrLn "Creating config..."
  mainBranch <- getRepoDefaultBranch org repo
  updatedAt  <- cast <$> time
  let ephemeral = MkEphem {
      filepath = "./\{Config.filename}"
    , colors   = terminalColors
    , editor
    }
  do teamSlugs  <- listTeams org
     repoLabels <- listRepoLabels org repo
     orgMembers <- listOrgMembers org
     let config = MkConfig {
         updatedAt
       , org
       , repo
       , defaultRemote
       , mainBranch
       , requestTeams
       , requestUsers
       , commentOnRequest
       , teamSlugs
       , repoLabels
       , orgMembers
       , ignoredPRs = []
       , githubPAT = hide <$> configPAT
       , ephemeral
       }
     ignore $ writeConfig config
     putStrLn "Your new configuration is:"
     printLn config
     either renderIO pure (checkConfigConsistency config)
     pure config
  where
    orIfEmpty : Maybe String -> String -> String
    orIfEmpty Nothing  x  = x
    orIfEmpty (Just y) "" = y
    orIfEmpty (Just _) x  = x

    org : Maybe GitRemote -> Maybe String
    org = map (.org)

    repo : Maybe GitRemote -> Maybe String
    repo = map (.repo)

    enterForDefaultStr : String -> String
    enterForDefaultStr str = " (ENTER for default: \{str})"

    defaultStr : (GitRemote -> String) -> Maybe GitRemote -> String
    defaultStr f = fromMaybe "" . map (enterForDefaultStr . f)

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

export
covering
loadConfig : HasIO io => 
             (terminalColors : Bool)
          -> (editor : Maybe String)
          -> io (Either ConfigError Config)
loadConfig terminalColors editor = let (>>=) = (>>=) @{Monad.Compose} in
  do location   <- mapFst File . maybeToEither FileNotFound <$>
                     findConfig "." (limit 10)
     configFile <- mapFst File <$> 
                     readFile location
     pure . mapFst Parse $ parseConfig (MkEphem location terminalColors editor) configFile

export
covering
loadOrCreateConfig : Git =>
                     (envGithubPAT : Maybe String)
                  -> (terminalColors : Bool)
                  -> (editor : Maybe String)
                  -> Promise Config
loadOrCreateConfig envGithubPAT terminalColors editor = do
  Right config <- loadConfig terminalColors editor
    | Left (File FileNotFound) => createConfig envGithubPAT terminalColors editor
    | Left err => reject "Error loading \{Config.filename}: \{show err}."
  pure config

