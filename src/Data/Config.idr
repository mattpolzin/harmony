module Data.Config

import Data.Either
import Data.List
import Data.List.Elem
import Data.String
import Data.Theme
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

import public Data.DPair

import Language.Reflection
import Util.Reflection

%language ElabReflection

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
  ||| The number of columns available in the terminal (at launch).
  columns  : Nat
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
  ||| The remote name (e.g. "origin")
  defaultRemote : String
  ||| The main branch. New PRs are based off of this branch.
  mainBranch    : String
  ||| True to request review from teams as well as individual users on PRs.
  requestTeams   : Bool
  ||| True to request review from users as well as teams on PRs.
  requestUsers   : Bool
  ||| True to comment on PRs after requesting review from users.
  commentOnRequest : Bool
  ||| Local cache of GitHub teams within the configured org.
  teamSlugs     : List String
  ||| Local cache of GitHub labels for the configured repo.
  repoLabels    : List String
  ||| Local cache of GitHub members within the configured org.
  orgMembers    : List String
  ||| A list of Pull Request numbers that should be ignored
  ||| (specifically when determining the results of the
  ||| contribute command).
  ignoredPRs    : List Integer
  ||| A GitHub Personal Access Token. This value is only used if
  ||| there is no $GITHUB_PAT environment variable set. One of
  ||| either the environment variable or this config property
  ||| must be set.
  githubPAT     : Maybe (Hidden String)
  ||| Should Harmony print with colors fit for a dark terminal
  ||| or a light terminal?
  theme         : Maybe Theme
  ||| Configuration properties that are not written to a file.
  ephemeral     : Ephemeral -- not written out to file

%name Config config

--
-- Settable Properties Setup
--

public export
data SettableProp : (name : String) -> (help : String) -> Type where
  RequestTeams     : SettableProp
    "requestTeams"
    "[true/false] Determines whether or not to request reviews from teams when requesting individual reviewers from a team."
  RequestUsers     : SettableProp
    "requestUsers"
    """
    [true/false] Determines whether or not to request reviews from an individual user based on Harmony's heuristics when \
    requestin review from teams. You might want to disable `requestUsers` to allow GitHub to pick users to request based on \
    the team. This setting does not affect the ability to request reviews from individual users withe Harmony's `+<username>` \
    syntax.
    """
  CommentOnRequest : SettableProp
    "commentOnRequest"
    "[true/false] Determines whether to comment on PR indicating that Harmony chose a reviewer."
  DefaultRemote   : SettableProp
    "defaultRemote"
    "[string]     The name of the default Git remote to use (e.g. 'origin')."
  MainBranch      : SettableProp
    "mainBranch"
    "[string]     The name of the default Git base branch for new PRs."
  ThemeProp       : SettableProp
    "theme"
    "[dark/light]"
  GithubPAT       : SettableProp
    "githubPAT"
    """
    [string]     The Personal Access Token Harmony should use to authenticate with GitHub. You can leave this unset if you \
    want to set a PAT via the GITHUB_PAT environment variable.
    """

  -- TODO 5.0.0: remove deprecated aliases below
  AssignTeams : SettableProp 
    "assignTeams"
    "[true/false] Deprecated alias for the 'requestTeams' config option."
  AssignUsers : SettableProp
    "assignUsers"
    "[true/false] Deprecated alias for the 'requestUsers' config option."
  CommentOnAssign : SettableProp
    "commentOnAssign"
    "[true/false] Deprecated alias for the 'commentOnRequest' config option."

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
settablePropNamed "requestTeams"     = Just $ Evidence _ RequestTeams
settablePropNamed "commentOnRequest" = Just $ Evidence _ CommentOnRequest
settablePropNamed "defaultRemote"    = Just $ Evidence _ DefaultRemote
settablePropNamed "mainBranch"       = Just $ Evidence _ MainBranch
settablePropNamed "theme"            = Just $ Evidence _ ThemeProp
settablePropNamed "githubPAT"        = Just $ Evidence _ GithubPAT
settablePropNamed "requestUsers"     = Just $ Evidence _ RequestUsers
settablePropNamed "assignTeams"      = Just $ Evidence _ AssignTeams
settablePropNamed "assignUsers"      = Just $ Evidence _ AssignUsers
settablePropNamed "commentOnAssign"  = Just $ Evidence _ CommentOnAssign
settablePropNamed _ = Nothing

namespace SettablePropNamedProperties
  settablePropNamedOnto : (p : SettableProp n h) -> Config.settablePropNamed n === (Just $ Evidence h p)
  settablePropNamedOnto prop = %runElab ( do
      cons <- getCons `{ Data.Config.SettableProp }
      check $ elabCase `( prop ) `( Data.Config.SettableProp n h )
                       cons (\name => (MkClause name `( Refl )))
    )

settableProps : List SomeSettableProp
settableProps = [
    (_ ** _ ** RequestTeams)
  , (_ ** _ ** RequestUsers)
  , (_ ** _ ** CommentOnRequest)
  , (_ ** _ ** DefaultRemote)
  , (_ ** _ ** MainBranch)
  , (_ ** _ ** ThemeProp)
  , (_ ** _ ** GithubPAT)
  , (_ ** _ ** AssignUsers)
  , (_ ** _ ** AssignTeams)
  , (_ ** _ ** CommentOnAssign)
  ]

namespace SettablePropsProperties
  settablePropsCovering : (p : SettableProp n h) -> Elem (n ** h ** p) Config.settableProps
  settablePropsCovering prop = %runElab ( do
      cons <- getCons `{ Data.Config.SettableProp }
      check $ elabCase `( prop ) `( Data.Config.SettableProp n h )
                       cons (\name => (MkClause name `( %search )))
    )

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

--
-- Accessors
-- 

export
(.filepath) : Config -> String
config.filepath = config.ephemeral.filepath

export
(.colors) : Config -> Bool
config.colors = config.ephemeral.colors

export
(.columns) : Config -> Nat
config.columns = config.ephemeral.columns

export
(.editor) : Config -> Maybe String
config.editor = config.ephemeral.editor

--
-- Show
--

export
Show Config where
  show config = unlines [
      "       updatedAt: \{show config.updatedAt}"
    , "           theme: \{show $ maybe Dark id config.theme}"
    , "             org: \{show config.org}"
    , "            repo: \{show config.repo}"
    , "   defaultRemote: \{show config.defaultRemote}"
    , "      mainBranch: \{show config.mainBranch}"
    , "    requestTeams: \{show config.requestTeams}"
    , "    requestUsers: \{show config.requestUsers}"
    , "commentOnRequest: \{show config.commentOnRequest}"
    , "       teamSlugs: \{show config.teamSlugs}"
    , "      repoLabels: \{show config.repoLabels}"
    , "      orgMembers: \{show config.orgMembers}"
    , "      ignoredPRs: \{show config.ignoredPRs}"
    , "       githubPAT: \{personalAccessToken}"
    ]
      where
        personalAccessToken : String
        personalAccessToken = maybe "Not set (will use $GITHUB_PAT environment variable)" show config.githubPAT

--
-- JSON Serialization
--

export
json : Config -> JSON
json (MkConfig updatedAt org repo defaultRemote mainBranch
               requestTeams requestUsers commentOnRequest teamSlugs
               repoLabels orgMembers ignoredPRs githubPAT theme _) =
  JObject [
      ("requestTeams"    , JBool requestTeams)
    , ("requestUsers"    , JBool requestUsers)
    , ("commentOnRequest", JBool commentOnRequest)
    , ("org"             , JString org)
    , ("repo"            , JString repo)
    , ("defaultRemote"   , JString defaultRemote)
    , ("mainBranch"      , JString mainBranch)
    , ("theme"           , JString . show $ maybe Dark id theme)
    , ("orgMembers"      , JArray $ JString <$> sort orgMembers)
    , ("teamSlugs"       , JArray $ JString <$> sort teamSlugs)
    , ("repoLabels"      , JArray $ JString <$> sort repoLabels)
    , ("ignoredPRs"      , JArray $ JInteger . cast <$> sort ignoredPRs)
    , ("githubPAT"       , maybe JNull (JString . expose) githubPAT)
    , ("updatedAt"       , JInteger $ cast updatedAt)
    ]

--
-- JSON Parsing
--

export
parseConfig : (ephemeral : Ephemeral) -> (filecontents : String) -> Either String Config
parseConfig ephemeral = (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseConfigJson
  where
    parseConfigJson : JSON -> Either String Config
    parseConfigJson (JObject config) = do [   updatedAt
                                            , org
                                            , repo
                                            , mainBranch
                                            , teamSlugs
                                            , orgMembers
                                            , defaultRemote
                                            , repoLabels
                                            , ignoredPRs
                                            ] <-
                                            lookupAll [
                                                "updatedAt"
                                              , "org"
                                              , "repo"
                                              , "mainBranch"
                                              , "teamSlugs"
                                              , "orgMembers"
                                              , "defaultRemote"
                                              , "repoLabels"
                                              , "ignoredPRs"
                                              ] config
                                          -- TODO 5.0.0: Stop supporting the old aliases "assign..." and move the 
                                          --             new "request..." versions into the required lookup above.
                                          requestTeams <- exactlyOneOf "assignTeams" "requestTeams"
                                          requestUsers <- exactlyOneOf "assignUsers" "requestUsers"
                                          commentOnRequest <- exactlyOneOf "commentOnAssign" "commentOnRequest"
                                          let maybeGithubPAT = lookup "githubPAT" config
                                          let maybeTheme = lookup "theme" config
                                          ua <- cast <$> integer updatedAt
                                          o  <- string org
                                          r  <- string repo
                                          dr <- string defaultRemote
                                          mb <- string mainBranch
                                          at <- bool requestTeams
                                          au <- bool requestUsers
                                          ca <- bool commentOnRequest
                                          ts <- array string teamSlugs
                                          rl <- array string repoLabels
                                          om <- array string orgMembers
                                          ip <- array integer ignoredPRs
                                          gp <- maybe (Right Nothing) (optional string) maybeGithubPAT
                                          th <- maybe (Right $ Just Dark) 
                                                      (optional $ stringy "dark or light" parseString) 
                                                      maybeTheme
                                          pure $ MkConfig {
                                              updatedAt         = ua
                                            , org               = o
                                            , repo              = r
                                            , defaultRemote     = dr
                                            , mainBranch        = mb
                                            , requestTeams      = at
                                            , requestUsers      = au
                                            , teamSlugs         = ts
                                            , repoLabels        = rl
                                            , commentOnRequest  = ca
                                            , orgMembers        = om
                                            , ignoredPRs        = ip
                                            , githubPAT         = (map Hide) gp
                                            , theme             = th
                                            , ephemeral         = ephemeral
                                            }
      where
        exactlyOneOf : String -> String -> Either String JSON
        exactlyOneOf key1 key2 = do
          let maybeKey1 = lookup key1 config
          let maybeKey2 = lookup key2 config
          case (maybeKey1, maybeKey2) of
               (Nothing, Nothing) => Left "Expected config JSON to contain either the '\{key1}' key (deprecated) or the '\{key2}' key (newer)."
               (Nothing, (Just value)) => Right value
               ((Just value), Nothing) => Right value
               ((Just _), (Just _)) => Left "Expected config JSON to contain only one of the '\{key1}' key (deprecated) or the '\{key2}' key (newer). Found values for both."

    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

export
filename : String
filename = "harmony.json"

