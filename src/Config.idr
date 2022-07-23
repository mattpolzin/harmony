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
import Language.JSON
import System
import System.File

%default total

writeConfig : Config -> Promise Config
writeConfig config =
  do res <- writeFile config.filepath (format 2 $ json config)
     case res of
          Right () => pure config
          Left err => reject "Failed to write updated config file to \{config.filepath}: \{show err}."

export
syncConfig : Config => Octokit => (echo : Bool) -> Promise Config
syncConfig @{config} echo =
 do teamSlugs  <- listTeams config.org
    orgMembers <- listOrgMembers config.org
    updatedAt  <- cast {to=Timestamp} <$> time
    let config' = { updatedAt  := updatedAt
                  , teamSlugs  := teamSlugs
                  , orgMembers := orgMembers
                  } config
    ignore $ writeConfig config'
    when echo $
      do putStrLn "Your updated configuration is:"
         printLn config
    pure config'

export
syncIfOld : Octokit => Config -> Promise Config
syncIfOld config =
  if config.updatedAt < !oneDayAgo
     then do -- putStrLn "Syncing config file..."
             syncConfig False
     else pure config
  where
    oneDayAgo : HasIO io => io Timestamp
    oneDayAgo =
      do let oneDay = 86_400
         now <- time
         pure $ cast (now - oneDay)

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

propSetters : List (String, (Config -> String -> Maybe Config))
propSetters = [
    ("assignTeams"    , update parseBool (\b => { assignTeams := b }))
  , ("commentOnAssign", update parseBool (\b => { commentOnAssign := b }))
  , ("defaultRemote"  , update Just (\s => { defaultRemote := Just s }))
  , ("githubPAT"      , update Just (\s => { githubPAT := Just $ hide s }))
  ]
  where
    parseBool : String -> Maybe Bool
    parseBool x with (toLower x)
      _ | "yes"   = Just True
      _ | "true"  = Just True
      _ | "no"    = Just False
      _ | "false" = Just False
      _ | _ = Nothing

    update : Functor f => (String -> f a) -> (a -> b -> b) -> b -> String -> f b
    update f g c = map (flip g c) . f

namespace PropSettersProperties
  propSettersCoveragePrf : Data.Config.settableProps = Builtin.fst <$> Config.propSetters
  propSettersCoveragePrf = Refl

||| Attempt to set a property and value given String representations.
||| After setting, write the config and return the updated result.
export
setConfig : Config =>
            (prop : String)
         -> (value : String)
         -> Promise Config
setConfig @{config} prop value with (lookup prop propSetters)
  _ | Nothing  = reject "\{prop} cannot be set via `config` command."
  _ | (Just set) with (set config value)
    _ | Nothing  = reject "\{value} is not a valid value for \{prop}."
    _ | (Just config') = writeConfig config'

propGetters : List (String, (Config -> String))
propGetters = [
    ("assignTeams"    , show . assignTeams)
  , ("commentOnAssign", show . commentOnAssign)
  , ("defaultRemote"  , maybe "Not set (defaults to \"origin\")" show . defaultRemote)
  , ("githubPAT"  , maybe "Not set (will use $GITHUB_PAT environment variable)" show . githubPAT)
  ]

namespace PropGettersProperties
  propGetterCoveragePrf : Data.Config.settableProps = Builtin.fst <$> Config.propGetters
  propGetterCoveragePrf = Refl

export
getConfig : Config =>
            (prop : String)
         -> Promise String
getConfig @{config} prop with (lookup prop propGetters)
  getConfig prop | Nothing    = reject "\{prop} cannot get read via `config` command."
  getConfig prop | (Just get) = pure $ get config

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
                   , "What PAT would you like to set in the config file \{defaultPATString}?"
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
  defaultRemote <- Just . orIfEmpty (Just remoteGuess) . trim <$> getLine
  
  putStr "Would you like harmony to comment when it assigns reviewers? [Y/n] "
  commentOnAssign <- yesUnlessNo . trim <$> getLine

  putStr "Would you like harmony to assign teams in addition to individuals when it assigns reviewers? [Y/n] "
  assignTeams <- yesUnlessNo . trim <$> getLine

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
     orgMembers <- listOrgMembers org
     let config = MkConfig {
         updatedAt
       , org
       , repo
       , defaultRemote
       , mainBranch
       , assignTeams
       , commentOnAssign
       , teamSlugs
       , orgMembers
       , githubPAT = hide <$> configPAT
       , ephemeral
       }
     ignore $ writeConfig config
     putStrLn "Your new configuration is:"
     printLn config
     pure config
  where
    orIfEmpty : Maybe String -> String -> String
    orIfEmpty Nothing  x  = x
    orIfEmpty (Just y) "" = y
    orIfEmpty (Just _) x  = x

    yesUnlessNo : String -> Bool
    yesUnlessNo "n" = False
    yesUnlessNo "N" = False
    yesUnlessNo _   = True

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

