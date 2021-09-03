module Data.Config

import Data.Either
import Data.List
import Data.String
import Data.Vect
import Language.JSON
import Language.JSON.Accessors

%default total

public export
Timestamp : Type
Timestamp = Bits32

public export
record Config where
  constructor MkConfig
  updatedAt  : Timestamp
  org        : String
  repo       : String
  mainBranch : String
  teamSlugs  : List String

%name Config config

export
Show Config where
  show config = unlines [
      "updatedAt: \{show config.updatedAt}"
    , "org: \{show config.org}"
    , "repo: \{show config.repo}"
    , "mainBranch: \{show config.mainBranch}"
    , "teamSlugs: \{show config.teamSlugs}"
    ]

export
json : Config -> JSON
json (MkConfig updatedAt org repo mainBranch teamSlugs) = 
  JObject [
      ("mainBranch", JString mainBranch)
    , ("org"       , JString org)
    , ("repo"      , JString repo)
    , ("teamSlugs" , JArray $ JString <$> sort teamSlugs)
    , ("updatedAt" , JNumber $ cast updatedAt)
    ]

export
parseConfig : String -> Either String Config
parseConfig = (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parseConfigJson
  where
    parseConfigJson : JSON -> Either String Config
    parseConfigJson (JObject config) = do [updatedAt, org, repo, mainBranch, teamSlugs] <-
                                            lookupAll ["updatedAt", "org", "repo", "mainBranch", "teamSlugs"] config
                                          ua <- cast <$> integer updatedAt
                                          o <- string org
                                          r <- string repo
                                          mb <- string mainBranch
                                          ts <- array teamSlugs string
                                          pure $ MkConfig {
                                              updatedAt  = ua
                                            , org        = o
                                            , repo       = r
                                            , mainBranch = mb
                                            , teamSlugs  = ts
                                            }
    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

export
filename : String
filename = "harmony.json"

