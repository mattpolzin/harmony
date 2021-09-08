module Data.Config

import Data.Either
import Data.List
import Data.String
import Data.Vect
import Language.JSON
import Language.JSON.Accessors

%default total

||| Unix Timestamp (seconds since epoch).
public export
Timestamp : Type
Timestamp = Bits32

public export
record Ephemeral where
  constructor MkEphem
  filepath : String
  colors   : Bool

public export
record Config where
  constructor MkConfig
  updatedAt  : Timestamp
  org        : String
  repo       : String
  mainBranch : String
  teamSlugs  : List String
  orgMembers : List String
  ephemeral  : Ephemeral -- not written out to file

%name Config config

export
(.filepath) : Config -> String
config.filepath = config.ephemeral.filepath

export
(.colors) : Config -> Bool
config.colors = config.ephemeral.colors

export
Show Config where
  show config = unlines [
      "updatedAt: \{show config.updatedAt}"
    , "org: \{show config.org}"
    , "repo: \{show config.repo}"
    , "mainBranch: \{show config.mainBranch}"
    , "teamSlugs: \{show config.teamSlugs}"
    , "orgMembers: \{show config.orgMembers}"
    ]

export
json : Config -> JSON
json (MkConfig updatedAt org repo mainBranch teamSlugs orgMembers _) = 
  JObject [
      ("mainBranch", JString mainBranch)
    , ("org"       , JString org)
    , ("orgMembers", JArray $ JString <$> sort orgMembers)
    , ("repo"      , JString repo)
    , ("teamSlugs" , JArray $ JString <$> sort teamSlugs)
    , ("updatedAt" , JNumber $ cast updatedAt)
    ]

export
parseConfig : (ephemeral : Ephemeral) -> (filecontents : String) -> Either String Config
parseConfig ephemeral = (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parseConfigJson
  where
    parseConfigJson : JSON -> Either String Config
    parseConfigJson (JObject config) = do [updatedAt, org, repo, mainBranch, teamSlugs, orgMembers] <-
                                            lookupAll ["updatedAt"
                                                      , "org"
                                                      , "repo"
                                                      , "mainBranch"
                                                      , "teamSlugs"
                                                      , "orgMembers"
                                                      ] config
                                          ua <- cast <$> integer updatedAt
                                          o <- string org
                                          r <- string repo
                                          mb <- string mainBranch
                                          ts <- array string teamSlugs 
                                          om <- array string orgMembers
                                          pure $ MkConfig {
                                              updatedAt  = ua
                                            , org        = o
                                            , repo       = r
                                            , mainBranch = mb
                                            , teamSlugs  = ts
                                            , orgMembers = om
                                            , ephemeral  = ephemeral
                                            }
    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

export
filename : String
filename = "harmony.json"

