module Config

import Data.Config
import Data.Either
import Data.List
import Data.List1
import Data.List.PrefixSuffix
import Data.Promise
import Data.String
import Decidable.Equality
import FFI.Git
import FFI.GitHub
import Language.JSON
import System
import System.File

%default total

writeConfig : Config -> Promise ()
writeConfig config =
  do res <- writeFile config.filepath (format 2 $ json config)
     case res of
          Right () => pure ()
          Left err => reject "Failed to write updated config file to \{config.filepath}: \{show err}."

export
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

export
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

createConfig : Git => Octokit => 
               (terminalColors : Bool)
            -> Promise Config
createConfig terminalColors = 
  do putStrLn "Creating a new configuration (storing in \{Config.filename})..."
     -- TODO: don't assume remote name ("origin"), get it from git or ask for it.
     defaultOrgAndRepo <- (parseGitHubURI <$> remoteURI "origin") <|> pure Nothing
     let orgDefaultStr = defaultStr (.org) defaultOrgAndRepo
     putStrLn "What GitHub org would you like to use harmony for\{orgDefaultStr}?"
     org  <- orIfEmpty (org defaultOrgAndRepo)  . trim <$> getLine
     let repoDefaultStr = defaultStr (.repo) defaultOrgAndRepo
     putStrLn "What repository would you like to use harmony for\{repoDefaultStr}?"
     repo <- orIfEmpty (repo defaultOrgAndRepo) . trim <$> getLine
     putStrLn "Creating config..."
     mainBranch <- getRepoDefaultBranch org repo
     updatedAt  <- cast <$> time
     let ephemeral = MkEphem {
         filepath = "./\{Config.filename}"
       , colors   = terminalColors
       }
     do teamSlugs  <- listTeams org
        orgMembers <- listOrgMembers org
        let config = MkConfig {
            updatedAt
          , org
          , repo
          , mainBranch
          , teamSlugs
          , orgMembers
          , ephemeral
          }
        writeConfig config
        putStrLn "Your new configuration is:"
        printLn config
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
          -> io (Either ConfigError Config)
loadConfig terminalColors = let (>>=) = (>>=) @{Monad.Compose} in
  do location   <- mapFst File . maybeToEither FileNotFound <$>
                     findConfig "." (limit 10)
     configFile <- mapFst File <$> 
                     readFile location
     pure . mapFst Parse $ parseConfig (MkEphem location terminalColors) configFile

export
covering
loadOrCreateConfig : Git => Octokit => 
                     (terminalColors : Bool)
                  -> Promise Config
loadOrCreateConfig terminalColors = 
  do Right config <- loadConfig terminalColors
       | Left (File FileNotFound) => createConfig terminalColors
       | Left err => reject "Error loading \{Config.filename}: \{show err}."
     pure config
