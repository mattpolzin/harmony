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
  ||| The file path where the current configuration should be
  ||| persisted.
  filepath : String
  ||| True to use ANSI terminal colors.
  colors   : Bool
  ||| If set, a preferred editor to use for writing PR desriptions.
  editor   : Maybe String

public export
record Config where
  constructor MkConfig
  ||| Timestamp when the config was last syncronized with GitHub.
  updatedAt   : Timestamp
  org         : String
  repo        : String
  ||| The main branch. New PRs are based off of this branch.
  mainBranch  : String
  ||| True to assign teams as well as individual users to PRs.
  assignTeams : Bool
  ||| True to comment on PRs after assigning users.
  commentOnAssign : Bool
  ||| Local cache of GitHub teams within the configured org.
  teamSlugs   : List String
  ||| Local cache of GitHub members within the configured org.
  orgMembers  : List String
  ||| Configuration properties that are not written to a file.
  ephemeral   : Ephemeral -- not written out to file

%name Config config

public export
settableProps : List String
settableProps = [
    "assignTeams"
  , "commentOnAssign"
  ]

export
(.filepath) : Config -> String
config.filepath = config.ephemeral.filepath

export
(.colors) : Config -> Bool
config.colors = config.ephemeral.colors

export
(.editor) : Config -> Maybe String
config.editor = config.ephemeral.editor

export
Show Config where
  show config = unlines [
      "      updatedAt: \{show config.updatedAt}"
    , "            org: \{show config.org}"
    , "           repo: \{show config.repo}"
    , "     mainBranch: \{show config.mainBranch}"
    , "    assignTeams: \{show config.assignTeams}"
    , "commentOnAssign: \{show config.commentOnAssign}"
    , "      teamSlugs: \{show config.teamSlugs}"
    , "     orgMembers: \{show config.orgMembers}"
    ]

export
json : Config -> JSON
json (MkConfig updatedAt org repo mainBranch assignTeams commentOnAssign teamSlugs orgMembers _) = 
  JObject [
      ("mainBranch"     , JString mainBranch)
    , ("assignTeams"    , JBoolean assignTeams)
    , ("commentOnAssign", JBoolean commentOnAssign)
    , ("org"            , JString org)
    , ("orgMembers"     , JArray $ JString <$> sort orgMembers)
    , ("repo"           , JString repo)
    , ("teamSlugs"      , JArray $ JString <$> sort teamSlugs)
    , ("updatedAt"      , JNumber $ cast updatedAt)
    ]

export
parseConfig : (ephemeral : Ephemeral) -> (filecontents : String) -> Either String Config
parseConfig ephemeral = (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parseConfigJson
  where
    parseConfigJson : JSON -> Either String Config
    parseConfigJson (JObject config) = do [   updatedAt
                                            , org
                                            , repo
                                            , mainBranch
                                            , assignTeams
                                            , commentOnAssign
                                            , teamSlugs
                                            , orgMembers
                                            ] <-
                                            lookupAll [
                                                "updatedAt"
                                              , "org"
                                              , "repo"
                                              , "mainBranch"
                                              , "assignTeams"
                                              , "commentOnAssign"
                                              , "teamSlugs"
                                              , "orgMembers"
                                              ] config
                                          ua <- cast <$> integer updatedAt
                                          o  <- string org
                                          r  <- string repo
                                          mb <- string mainBranch
                                          at <- bool assignTeams
                                          ca <- bool commentOnAssign
                                          ts <- array string teamSlugs 
                                          om <- array string orgMembers
                                          pure $ MkConfig {
                                              updatedAt        = ua
                                            , org              = o
                                            , repo             = r
                                            , mainBranch       = mb
                                            , assignTeams      = at
                                            , teamSlugs        = ts
                                            , commentOnAssign  = ca
                                            , orgMembers       = om
                                            , ephemeral        = ephemeral
                                            }
    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

export
filename : String
filename = "harmony.json"

