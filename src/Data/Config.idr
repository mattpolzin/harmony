module Data.Config

import Data.Either
import Data.Issue
import Data.List
import Data.List.Elem
import Data.Project
import Data.String
import Data.Theme
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

import public Data.DPair

import Language.Reflection
import Util.Reflection

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%language ElabReflection

%default total

||| Unix Timestamp (seconds since epoch).
public export
Timestamp : Type
Timestamp = Bits32

public export
record Ephemeral where
  constructor MkEphem
  ||| The file path where the current configuration should be persisted.
  filepath  : String
  ||| True if the stdout file is a TTY terminal.
  ttyStdout : Bool
  ||| True to use ANSI terminal colors.
  colors    : Bool
  ||| The number of columns available in the terminal (at launch).
  columns   : Nat
  ||| If set, a preferred editor to use for writing PR desriptions.
  editor    : Maybe String

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

namespace CommentStrategy
  public export
  data CommentStrategy = None | Name | AtMention

  export
  Show CommentStrategy where
    show None = "none"
    show Name = "name"
    show AtMention = "at-mention"

  export
  parseCommentConfig : String -> Maybe CommentStrategy
  parseCommentConfig "none" = Just None
  parseCommentConfig "name" = Just Name
  parseCommentConfig "at-mention" = Just AtMention
  parseCommentConfig "atmention" = Just AtMention
  parseCommentConfig _ = Nothing

commentConfig : JSON -> Either String CommentStrategy
commentConfig = (map toLower . string)  >=> (maybeToEither "" . parseCommentConfig)
  where
    err : String
    err = "Expected the commentOnRequest setting to be 'none', 'name', or 'at-mention'"

namespace ParseBranchStrategy
  public export
  data ParseBranchStrategy = None | Jira | Github

  export
  Show ParseBranchStrategy where
    show None   = "none"
    show Jira   = "jira"
    show Github = "github"

  export
  Eq ParseBranchStrategy where
    None   == None   = True
    Jira   == Jira   = True
    Github == Github = True
    _ == _ = False

  export
  parseBranchConfig : String -> Maybe ParseBranchStrategy
  parseBranchConfig "none"   = Just None
  parseBranchConfig "jira"   = Just Jira
  parseBranchConfig "github" = Just Github
  parseBranchConfig _ = Nothing

branchConfig : JSON -> Either String ParseBranchStrategy
branchConfig = (map toLower . string)  >=> (maybeToEither "" . parseBranchConfig)
  where
    err : String
    err = "Expected the branchParsing setting to be 'none', 'github', or 'jira'"

Alternative (Either String) where
  empty = Left "empty"

  (Right x) <|> _ = Right x
  (Left x) <|> b = b

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
  ||| The project to add new issues to by default (if any).
  defaultProject     : Maybe ProjectRef
  ||| The parent issue (number) to add new issues to by default (if any).
  defaultParentIssue : Maybe Integer
  ||| True to request review from teams as well as individual users on PRs.
  requestTeams : Bool
  ||| True to request review from users as well as teams on PRs.
  requestUsers : Bool
  ||| AtMention or Name to comment on PRs after requesting review from users.
  commentOnRequest : CommentStrategy
  ||| If set to Jira, attempt to extract a Jira slug from branch names and use
  ||| that in the PR title.
  |||
  ||| If set to Github, attempt to extract a Github issue number from branch
  ||| names and use that in the PR body.
  branchParsing : ParseBranchStrategy
  ||| If set, prefix new PR titles (by default suggestion only) with the given
  ||| string. For example, '[fix]'.
  bugfixPRTitlePrefix : Maybe String
  ||| Add a PR tree/chain to the PR body when creating a new PR that is not
  ||| `--into` the `mainBranch`.
  |||
  ||| This looks like e.g.
  |||   > ⨀ `main`
  |||   >> ↖ `feature-1` (https://github.com/org/repo/pull/123)
  |||   >> **Fancy PR**
  |||   >>> ↖ `feature-2`
  addPrTreeDescription : Bool
  ||| Local cache of GitHub teams within the configured org.
  teamSlugs     : List String
  ||| Local cache of GitHub labels for the configured repo.
  repoLabels    : List String
  ||| Local cache of GitHub projects for the configured repo.
  repoProjects  : List ProjectRef
  ||| Local cache of GitHub members within the configured org.
  orgMembers    : List String
  ||| A list of Pull Request numbers that should be ignored (specifically when
  ||| determining the results of the contribute command).
  ignoredPRs    : List Integer
  ||| A GitHub Personal Access Token. This value is only used if there is no
  ||| $GITHUB_PAT or $GH_TOKEN environment variable set. One of either the
  ||| environment variable or this config property must be set.
  githubPAT     : Maybe (Hidden String)
  ||| Generally not used for harmony actions which can rely on the concept of a
  ||| current (authenticated) user for any given GitHub request, but if shell
  ||| completion or some other action is only indirectly related to a GitHub
  ||| user then it may use this cached information.
  |||
  ||| This is the user's login or "slug"
  githubUser    : Maybe String
  ||| Should Harmony print with colors fit for a dark terminal or a light
  ||| terminal?
  theme         : Theme
  ||| Configuration properties that are not written to a file.
  ephemeral     : Ephemeral -- not written out to file

%name Config config

--
-- Settable Properties Setup
--

data Options = Enum (List String) | Any String

(.strValue) : Options -> String
(.strValue) (Enum strs) = concat $ intersperse "/" strs
(.strValue) (Any str) = str

(.enumeratedOptions) : Options -> List String
(.enumeratedOptions) (Enum strs) = strs
(.enumeratedOptions) (Any str) = []
--                               ^
-- The string here describes the type of option so it is not itself one of the options.

booleanOptions : Options
booleanOptions = Enum ["true", "false"]

Help : Type
Help = (Options, String)

public export
data SettableProp : (name : String) -> (help : Help) -> Type where
  RequestTeams     : SettableProp
    "requestTeams"
    ( Config.booleanOptions
    , """
      Determines whether or not to request reviews from teams when \
      requesting individual reviewers from a team.
      """
    )
  RequestUsers     : SettableProp
    "requestUsers"
    ( Config.booleanOptions
    , """
      Determines whether or not to request reviews from an individual \
      user based on Harmony's heuristics when requestin review from teams. You \
      might want to disable `requestUsers` to allow GitHub to pick users to \
      request based on the team. This setting does not affect the ability to \
      request reviews from individual users withe Harmony's `+<username>` \
      syntax.
      """
    )
  CommentOnRequest : SettableProp
    "commentOnRequest"
    ( (Enum ["none", "name", "at-mention"])
    , """
      Determines whether- and how to comment on PR \
      indicating that Harmony chose a reviewer.
      """
    )
  ParseBranchStrategy : SettableProp
    "branchParsing"
    ( (Enum ["none", "jira", "github"])
    , """
      Determines whether- and how to parse branch names for a prefix \
      to automatically add to a new PR's title or body to link the PR and \
      issue/ticket.
      """
    )
  BugfixPRTitlePrefix : SettableProp
    "bugfixPRTitlePrefix"
    ( (Any "string")
    , """
      A string to prefix default PR titles with when the branch the PR \
      is being created from is determined to be a bugfix branch (branch name \
      starts with 'bugfix'). For example, a common prefix is '[fix]'.
      """
    )
  AddPrTreeDescription : SettableProp
    "addPrTreeDescription"
    ( Config.booleanOptions
    , """
      Determines whether to add a tree of PRs to the description \
      for any PR that is into a branch other than the `mainBranch` configured.
      """
    )
  DefaultRemote      : SettableProp
    "defaultRemote"
    ( (Any "string")
    , "The name of the default Git remote to use (e.g. 'origin')."
    )
  MainBranch         : SettableProp
    "mainBranch"
    ( (Any "string")
    , "The name of the default Git base branch for new PRs."
    )
  DefaultProject     : SettableProp
    "defaultProject"
    ( (Any "number")
    , """
      The project number of a default project to add new issues to. \
      You can leave this unset if you don't want new issues added to any project.`
      """
    )
  DefaultParentIssue : SettableProp
    "defaultParentIssue"
    ( (Any "number")
    , """
      The issue number of a default issue to create new issues under. \
      You can leave this unset if you don't want new issues added to any parent issue.`
      """
    )
  ThemeProp          : SettableProp
    "theme"
    ( (Enum ["dark", "light"])
    , ""
    )
  GithubPAT          : SettableProp
    "githubPAT"
    ( (Any "string")
    , """
      The Personal Access Token Harmony should use to authenticate \
      with GitHub. You can leave this unset if you want to set a PAT via the \
      GITHUB_PAT or GH_TOKEN environment variable.
      """
    )

public export
SomeSettableProp : Type
SomeSettableProp = (n ** h ** SettableProp n h)

public export
propName : {n : _} -> SettableProp n h -> String
propName x = n

public export
propHelp : {h : _} -> SettableProp n h -> String
propHelp x = "[\{(fst h).strValue}] " ++ (snd h)

public export
propOptions : {h : _} -> SettableProp n h -> List String
propOptions x = (fst h).enumeratedOptions

export
settablePropNamed : (name : String) -> Maybe (Exists (SettableProp name))
settablePropNamed "requestTeams"         = Just $ Evidence _ RequestTeams
settablePropNamed "commentOnRequest"     = Just $ Evidence _ CommentOnRequest
settablePropNamed "branchParsing"        = Just $ Evidence _ ParseBranchStrategy
settablePropNamed "bugfixPRTitlePrefix"  = Just $ Evidence _ BugfixPRTitlePrefix
settablePropNamed "addPrTreeDescription" = Just $ Evidence _ AddPrTreeDescription
settablePropNamed "defaultRemote"        = Just $ Evidence _ DefaultRemote
settablePropNamed "mainBranch"           = Just $ Evidence _ MainBranch
settablePropNamed "defaultProject"       = Just $ Evidence _ DefaultProject
settablePropNamed "defaultParentIssue"   = Just $ Evidence _ DefaultParentIssue
settablePropNamed "theme"                = Just $ Evidence _ ThemeProp
settablePropNamed "githubPAT"            = Just $ Evidence _ GithubPAT
settablePropNamed "requestUsers"         = Just $ Evidence _ RequestUsers
settablePropNamed _ = Nothing

namespace SettablePropNamedProperties
  settablePropNamedOnto : (p : SettableProp n h) -> Config.settablePropNamed n === (Just $ Evidence h p)
  settablePropNamedOnto prop = %runElab ( do
      cons <- getCons `{ Data.Config.SettableProp }
      check $ elabCase `( prop ) `( Data.Config.SettableProp n h )
                       cons (\name => (MkClause name `( Refl )))
    )

export
reifyProp : Exists (SettableProp name) -> (h ** SettableProp name h)
reifyProp (Evidence h prop) = %runElab ( do
            cons <- getCons `{ Data.Config.SettableProp }
            check $ elabCase `( prop ) `( Data.Config.SettableProp name h )
                             cons (\name => let name' = IVar EmptyFC name
                                            in  (MkClause name `( (_ ** ~name') )))
          )

settableProps : List SomeSettableProp
settableProps = %runElab ( do
                  cons <- getCons `{ Data.Config.SettableProp }
                  let prop = (\name => let name' = IVar EmptyFC name
                                        in  `( (_ ** _ ** ~name') ))
                  traverse check (prop <$> cons)
                )

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
(.ttyStdout) : Config -> Bool
config.ttyStdout = config.ephemeral.ttyStdout

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
-- Show / Pretty
--

export 
render : Config -> Doc AnsiStyle
render config = vsep
  [ "           updatedAt:" <++> (pretty $ show config.updatedAt)
  , "               theme:" <++> (pretty $ show config.theme)
  , "         org or user:" <++> (pretty $ config.org)
  , "                repo:" <++> (pretty $ config.repo)
  , "       defaultRemote:" <++> (pretty $ config.defaultRemote)
  , "          mainBranch:" <++> (pretty $ config.mainBranch)
  , "      defaultProject:" <++> (pretty $ maybe "not set" show config.defaultProject)
  , "  defaultParentIssue:" <++> (pretty $ maybe "not set" show config.defaultParentIssue)
  , "        requestTeams:" <++> (pretty $ show config.requestTeams)
  , "        requestUsers:" <++> (pretty $ show config.requestUsers)
  , "    commentOnRequest:" <++> (pretty $ show config.commentOnRequest)
  , "       branchParsing:" <++> (pretty $ show config.branchParsing)
  , " bugfixPRTitlePrefix:" <++> (pretty $ maybe "Not set" show config.bugfixPRTitlePrefix)
  , "addPrTreeDescription:" <++> (pretty $ show config.addPrTreeDescription)
  , "           teamSlugs:" <++> (pretty $ newlineList config.teamSlugs)
  , "          repoLabels:" <++> (pretty $ newlineList $ show <$> config.repoLabels)
  , "        repoProjects:" <++> (pretty $ newlineList $ show <$> config.repoProjects)
  , "          orgMembers:" <++> (pretty $ newlineList $ config.orgMembers)
  , "          ignoredPRs:" <++> (pretty $ newlineList $ show <$> config.ignoredPRs)
  , "           githubPAT:" <++> (pretty $ personalAccessToken)
  , "          githubUser:" <++> (pretty $ maybe "not set" id config.githubUser)
  ]
      where
        personalAccessToken : String
        personalAccessToken = maybe "not set (will use $GITHUB_PAT or $GH_TOKEN environment variable)" show config.githubPAT

        spacer : String
        spacer = "                      "

        newlineList : List String -> String
        newlineList []   = "none"
        newlineList strs =
          let maxLength = foldr (max . String.length) 0 strs
              padding = 2 + maxLength
          in go padding strs

          where
            go : Nat -> List String -> String
            go _ [] = ""
            go _ [str] = str
            go padding (str1 :: str2 :: rest) = 
              let between = replicate (padding `minus` (length str1)) ' '
                  line = "\{str1}\{between}\{str2}"
              in  case rest of
                       [] => line
                       _ => line ++ "\n\{spacer}" ++ (go padding rest)

export
Show Config where
  show = renderString . layoutUnbounded . unAnnotate . render

--
-- JSON Serialization
--

export
json : Config -> JSON
json (MkConfig updatedAt org repo defaultRemote mainBranch defaultProject
               defaultParentIssue requestTeams requestUsers commentOnRequest
               branchParsing bugfixPRTitlePrefix addPrTreeDescription teamSlugs
               repoLabels repoProjects orgMembers ignoredPRs githubPAT
               githubUser theme _) =
  JObject [
      ("requestTeams"         , JBool requestTeams)
    , ("requestUsers"         , JBool requestUsers)
    , ("commentOnRequest"     , JString $ show commentOnRequest)
    , ("branchParsing"        , JString $ show branchParsing)
    , ("bugfixPRTitlePrefix"  , maybe JNull JString bugfixPRTitlePrefix)
    , ("addPrTreeDescription" , JBool addPrTreeDescription)
    , ("org"                  , JString org)
    , ("repo"                 , JString repo)
    , ("defaultRemote"        , JString defaultRemote)
    , ("mainBranch"           , JString mainBranch)
    , ("defaultProject"       , maybe JNull json defaultProject)
    , ("defaultParentIssue"   , maybe JNull JInteger defaultParentIssue)
    , ("theme"                , JString $ show theme)
    , ("orgMembers"           , JArray $ JString <$> sort orgMembers)
    , ("teamSlugs"            , JArray $ JString <$> sort teamSlugs)
    , ("repoLabels"           , JArray $ JString <$> sort repoLabels)
    , ("repoProjects"         , JArray $ json <$> sort repoProjects)
    , ("ignoredPRs"           , JArray $ JInteger . cast <$> sort ignoredPRs)
    , ("githubPAT"            , maybe JNull (JString . expose) githubPAT)
    , ("githubUser"           , maybe JNull JString githubUser)
    , ("updatedAt"            , JInteger $ cast updatedAt)
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
                                            , bugfixPrefix
                                            , repoLabels
                                            , ignoredPRs
                                            , requestTeams
                                            , requestUsers
                                            , commentOnRequest
                                            , branchParsing
                                            , theme
                                            ] <-
                                            lookupAll [
                                                "updatedAt"
                                              , "org"
                                              , "repo"
                                              , "mainBranch"
                                              , "teamSlugs"
                                              , "orgMembers"
                                              , "defaultRemote"
                                              , "bugfixPRTitlePrefix"
                                              , "repoLabels"
                                              , "ignoredPRs"
                                              , "requestTeams"
                                              , "requestUsers"
                                              , "commentOnRequest"
                                              , "branchParsing"
                                              , "theme"
                                              ] config
                                          let maybeGithubPAT = lookup "githubPAT" config
                                          let maybeGithubUser = lookup "githubUser" config
                                          let maybePrTree = lookup "addPrTreeDescription" config
                                          let maybeDefaultProject = lookup "defaultProject" config
                                          let maybeDefaultParentIssue = lookup "defaultParentIssue" config
                                          let maybeRepoProjects = lookup "repoProjects" config
                                          ua <- cast <$> integer updatedAt
                                          o  <- string org
                                          r  <- string repo
                                          dr <- string defaultRemote
                                          mb <- string mainBranch
                                          dp <- maybe (Right Nothing) (optional parseProjectRef) maybeDefaultProject
                                          -- TODO 9.0.0: Make defaultProject required part of config file (default to Nothing)
                                          --             defaultProject lookup can be moved to the required lookupAll above.
                                          at <- bool requestTeams
                                          dpi <- maybe (Right Nothing) (optional integer) maybeDefaultParentIssue
                                          -- TODO 9.0.0: Make defaultParentIssue required part of config file (default to Nothing)
                                          --             defaultParentIssue lookup can be moved to the required lookupAll above.
                                          au <- bool requestUsers
                                          ca <- commentConfig commentOnRequest
                                          bp <- branchConfig branchParsing
                                          prt <- maybe (Right False) bool maybePrTree
                                          -- TODO 8.0.0: Make addPrTreeDescription required part of config file (default to false)
                                          --             addPrTreeDescription lookup can be moved to the required lookupAll above.
                                          rp <- maybe (Right []) (array parseProjectRef) maybeRepoProjects
                                          -- TODO 9.0.0: Make repoProjects required part of config file (default to [])
                                          --             repoProjects lookup can be moved to the required lookupAll above.
                                          ts <- array string teamSlugs
                                          rl <- array string repoLabels
                                          om <- array string orgMembers
                                          ip <- array integer ignoredPRs
                                          gp <- maybe (Right Nothing) (optional string) maybeGithubPAT
                                          gu <- maybe (Right Nothing) (optional string) maybeGithubUser
                                          bf <- optional string bugfixPrefix
                                          th <- (stringy "dark or light" parseString) theme
                                          pure $ MkConfig {
                                              updatedAt            = ua
                                            , org                  = o
                                            , repo                 = r
                                            , defaultRemote        = dr
                                            , mainBranch           = mb
                                            , defaultProject       = dp
                                            , defaultParentIssue   = dpi
                                            , requestTeams         = at
                                            , requestUsers         = au
                                            , teamSlugs            = ts
                                            , repoLabels           = rl
                                            , repoProjects         = rp
                                            , commentOnRequest     = ca
                                            , branchParsing        = bp
                                            , bugfixPRTitlePrefix  = bf
                                            , addPrTreeDescription = prt
                                            , orgMembers           = om
                                            , ignoredPRs           = ip
                                            , githubPAT            = (map Hide) gp
                                            , githubUser           = gu
                                            , theme                = th
                                            , ephemeral            = ephemeral
                                            }
      where
        exactlyOneOf : String -> String -> Either String JSON
        exactlyOneOf key1 key2 = do
          let maybeKey1 = lookup key1 config
          let maybeKey2 = lookup key2 config
          case (maybeKey1, maybeKey2) of
               (Nothing, Nothing) => 
                 Left "Expected config JSON to contain either the '\{key1}' key (deprecated) or the '\{key2}' key (newer)."
               (Nothing, (Just value)) => Right value
               ((Just value), Nothing) => Right value
               ((Just _), (Just _)) => 
                 Left "Expected config JSON to contain only one of the '\{key1}' key (deprecated) or the '\{key2}' key (newer). Found values for both."

    parseConfigJson (JArray _) = Left "Expected config JSON to be an Object, not an array."
    parseConfigJson _          = Left "Expected config JSON to be an Object."

export
filename : String
filename = "harmony.json"

public export
simpleDefaults : Config
simpleDefaults = 
    MkConfig {
        updatedAt            = 0
      , org                  = "org"
      , repo                 = "repo"
      , defaultRemote        = "origin"
      , mainBranch           = "main"
      , defaultProject       = Nothing
      , defaultParentIssue   = Nothing
      , requestTeams         = True
      , requestUsers         = True
      , teamSlugs            = []
      , repoLabels           = []
      , repoProjects         = []
      , commentOnRequest     = None
      , branchParsing        = None
      , bugfixPRTitlePrefix  = Nothing
      , addPrTreeDescription = False
      , orgMembers           = []
      , ignoredPRs           = []
      , githubPAT            = Nothing
      , githubUser           = Nothing
      , theme                = Dark
      , ephemeral            = MkEphem "path/to/repo" False False 200 Nothing
      }

