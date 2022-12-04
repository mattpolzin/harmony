module Data.Config

import Data.Either
import Data.List
import Data.List.Elem
import Data.String
import Data.Vect
import Language.JSON
import Language.JSON.Accessors

import public Data.DPair

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
  -- TODO 3.0.0:         ^ remove optionality with version 3.0.0; until then, we will support this being absent to be non-breaking
  ||| The main branch. New PRs are based off of this branch.
  mainBranch    : String
  ||| True to assign teams as well as individual users to PRs.
  assignTeams   : Bool
  ||| True to assign users as well as teams to PRs.
  assignUsers   : Bool
  ||| True to comment on PRs after assigning users.
  commentOnAssign : Bool
  ||| Local cache of GitHub teams within the configured org.
  teamSlugs     : List String
  ||| Local cache of GitHub labels for the configured repo.
  repoLabels    : List String
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
data SettableProp : (name : String) -> (help : String) -> Type where
  AssignTeams     : SettableProp
    "assignTeams"
    "[true/false] Determines whether or not to assign teams when assigning individual reviewers from a team."
  AssignUsers     : SettableProp
    "assignUsers"
    "[true/false] Determines whether or not to assign an individual user based on Harmony's heuristics when assigning teams. You might want to disable `assginUsers` to allow GitHub to pick users to assign based on the team. This setting does not affect the ability to assign individual users withe Harmony's `+<username>` syntax."
  CommentOnAssign : SettableProp
    "commentOnAssign"
    "[true/false] Determines whether to comment on PR indicating that Harmony chose a reviewer."
  DefaultRemote   : SettableProp
    "defaultRemote"
    "[string]     The name of the default Git remote to use (e.g. 'origin')."
  GithubPAT       : SettableProp
    "githubPAT"
    "[string]     The Personal Access Token Harmony should use to authenticate with GitHub. You can leave this unset if you want to set a PAT via the GITHUB_PAT environment variable."

public export
SomeSettableProp : Type
SomeSettableProp = (n ** h ** SettableProp n h)

public export
propName : {n : _} -> SettableProp n h -> String
propName x = n

public export
propHelp : {h : _} -> SettableProp n h -> String
propHelp x = h

export
settablePropNamed : (name : String) -> Maybe (Exists (SettableProp name))
settablePropNamed "assignTeams"     = Just $ Evidence _ AssignTeams
settablePropNamed "commentOnAssign" = Just $ Evidence _ CommentOnAssign
settablePropNamed "defaultRemote"   = Just $ Evidence _ DefaultRemote
settablePropNamed "githubPAT"       = Just $ Evidence _ GithubPAT
settablePropNamed "assignUsers"     = Just $ Evidence _ AssignUsers
settablePropNamed _ = Nothing

namespace SettablePropNamedProps
  settablePropNamedOnto : {p : SettableProp n h} -> Config.settablePropNamed n === (Just $ Evidence h p)
  settablePropNamedOnto {p = AssignTeams}     = Refl
  settablePropNamedOnto {p = AssignUsers}     = Refl
  settablePropNamedOnto {p = CommentOnAssign} = Refl
  settablePropNamedOnto {p = DefaultRemote}   = Refl
  settablePropNamedOnto {p = GithubPAT}       = Refl

settableProps : List SomeSettableProp
settableProps = [
    (_ ** _ ** AssignTeams)
  , (_ ** _ ** AssignUsers)
  , (_ ** _ ** CommentOnAssign)
  , (_ ** _ ** DefaultRemote)
  , (_ ** _ ** GithubPAT)
  ]

namespace SettablePropsProps
  settablePropsCovering : {p : SettableProp n h} -> Elem (n ** h ** p) Config.settableProps
  settablePropsCovering {p = AssignTeams}     = %search
  settablePropsCovering {p = AssignUsers}     = %search
  settablePropsCovering {p = CommentOnAssign} = %search
  settablePropsCovering {p = DefaultRemote}   = %search
  settablePropsCovering {p = GithubPAT}       = %search

propName' : SomeSettableProp -> String
propName' (_ ** _ ** p) = propName p

propHelp' : SomeSettableProp -> String
propHelp' (_ ** _ ** p) = propHelp p

export
settablePropNames : List String
settablePropNames = propName' <$> settableProps

export
settablePropNamesAndHelp : List (String, String)
settablePropNamesAndHelp = (\p => (propName' p, propHelp' p)) <$> settableProps

export
longestSettablePropName : Nat
longestSettablePropName = foldr max 0 $ (length . propName') <$> settableProps

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
    , "    assignUsers: \{show config.assignUsers}"
    , "commentOnAssign: \{show config.commentOnAssign}"
    , "      teamSlugs: \{show config.teamSlugs}"
    , "     repoLabels: \{show config.repoLabels}"
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
json (MkConfig updatedAt org repo defaultRemote mainBranch assignTeams assignUsers commentOnAssign
               teamSlugs repoLabels orgMembers githubPAT _) = 
  JObject [
      ("assignTeams"    , JBoolean assignTeams)
    , ("assignUsers"    , JBoolean assignUsers)
    , ("commentOnAssign", JBoolean commentOnAssign)
    , ("org"            , JString org)
    , ("repo"           , JString repo)
    , ("defaultRemote"  , maybe JNull JString defaultRemote)
    , ("mainBranch"     , JString mainBranch)
    , ("orgMembers"     , JArray $ JString <$> sort orgMembers)
    , ("teamSlugs"      , JArray $ JString <$> sort teamSlugs)
    , ("repoLabels"     , JArray $ JString <$> sort repoLabels)
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
                                          let maybeAssignUsers = lookup "assignUsers" config
                                          -- TODO 3.0.0:  ^ remove optionality with version 3.0.0 by moving into above list of required props; until then, we will support this being absent to be non-breaking
                                          let maybeRepoLabels = lookup "repoLabels" config
                                          -- TODO 3.0.0:  ^ remove optionality with version 3.0.0 by moving into above list of required props; until then, we will support this being absent to be non-breaking
                                          let maybeDefaultRemote = lookup "defaultRemote" config
                                          let maybeGithubPAT = lookup "githubPAT" config
                                          ua <- cast <$> integer updatedAt
                                          o  <- string org
                                          r  <- string repo
                                          dr <- maybe (Right Nothing) (optional string) maybeDefaultRemote
                                          mb <- string mainBranch
                                          at <- bool assignTeams
                                          au <- maybe (Right True) bool maybeAssignUsers
                                          ca <- bool commentOnAssign
                                          ts <- array string teamSlugs
                                          rl <- maybe (Right []) (array string) maybeRepoLabels
                                          om <- array string orgMembers
                                          gp <- maybe (Right Nothing) (optional string) maybeGithubPAT
                                          pure $ MkConfig {
                                              updatedAt        = ua
                                            , org              = o
                                            , repo             = r
                                            , defaultRemote    = dr
                                            , mainBranch       = mb
                                            , assignTeams      = at
                                            , assignUsers      = au
                                            , teamSlugs        = ts
                                            , repoLabels       = rl
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

