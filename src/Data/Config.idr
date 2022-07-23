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

export
data Hidden a = Hide a

export
hide : a -> Hidden a
hide = Hide

export
expose : Hidden a -> a
expose (Hide x) = x

export
Show (Hidden a) where
  show _ = "xxxxxxxx (hidden)"

public export
record Config where
  constructor MkConfig
  ||| Timestamp when the config was last syncronized with GitHub.
  updatedAt     : Timestamp
  org           : String
  repo          : String
  ||| The remote name (e.g. "origin"). If unspecified, "origin" is assumed.
  defaultRemote : Maybe String
  -- TODO 2.0.0:         ^ remove optionality with version 2.0.0; until then, we will support this being absent to be non-breaking
  ||| The main branch. New PRs are based off of this branch.
  mainBranch    : String
  ||| True to assign teams as well as individual users to PRs.
  assignTeams   : Bool
  ||| True to comment on PRs after assigning users.
  commentOnAssign : Bool
  ||| Local cache of GitHub teams within the configured org.
  teamSlugs     : List String
  ||| Local cache of GitHub members within the configured org.
  orgMembers    : List String
  ||| A GitHub Personal Access Token. This value is only used if
  ||| there is no $GITHUB_PAT environment variable set. One of
  ||| either the environment variable or this config property
  ||| must be set.
  githubPAT     : Maybe (Hidden String)
  ||| Configuration properties that are not written to a file.
  ephemeral     : Ephemeral -- not written out to file

%name Config config

public export
settableProps : List String
settableProps = [
    "assignTeams"
  , "commentOnAssign"
  , "defaultRemote"
  , "githubPAT"
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
    , "  defaultRemote: \{defaultRemote}"
    , "     mainBranch: \{show config.mainBranch}"
    , "    assignTeams: \{show config.assignTeams}"
    , "commentOnAssign: \{show config.commentOnAssign}"
    , "      teamSlugs: \{show config.teamSlugs}"
    , "     orgMembers: \{show config.orgMembers}"
    , "      githubPAT: \{personalAccessToken}"
    ]
      where
        defaultRemote : String
        defaultRemote = maybe "Not set (defaults to \"origin\")" show config.defaultRemote

        personalAccessToken : String
        personalAccessToken = maybe "Not set (will use $GITHUB_PAT environment variable)" show config.githubPAT

export
json : Config -> JSON
json (MkConfig updatedAt org repo defaultRemote mainBranch assignTeams commentOnAssign teamSlugs orgMembers githubPAT _) = 
  JObject [
      ("assignTeams"    , JBoolean assignTeams)
    , ("commentOnAssign", JBoolean commentOnAssign)
    , ("org"            , JString org)
    , ("repo"           , JString repo)
    , ("defaultRemote"  , maybe JNull JString defaultRemote)
    , ("mainBranch"     , JString mainBranch)
    , ("orgMembers"     , JArray $ JString <$> sort orgMembers)
    , ("teamSlugs"      , JArray $ JString <$> sort teamSlugs)
    , ("githubPAT"      , maybe JNull (JString . expose) githubPAT)
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
                                          let maybeDefaultRemote = lookup "defaultRemote" config
                                          let maybeGithubPAT = lookup "githubPAT" config
                                          ua <- cast <$> integer updatedAt
                                          o  <- string org
                                          r  <- string repo
                                          dr <- maybe (Right Nothing) (optional string) maybeDefaultRemote
                                          mb <- string mainBranch
                                          at <- bool assignTeams
                                          ca <- bool commentOnAssign
                                          ts <- array string teamSlugs 
                                          om <- array string orgMembers
                                          gp <- maybe (Right Nothing) (optional string) maybeGithubPAT
                                          pure $ MkConfig {
                                              updatedAt        = ua
                                            , org              = o
                                            , repo             = r
                                            , defaultRemote    = dr
                                            , mainBranch       = mb
                                            , assignTeams      = at
                                            , teamSlugs        = ts
                                            , commentOnAssign  = ca
                                            , orgMembers       = om
                                            , githubPAT        = (map Hide) gp
                                            , ephemeral        = ephemeral
                                            }
    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

export
filename : String
filename = "harmony.json"

