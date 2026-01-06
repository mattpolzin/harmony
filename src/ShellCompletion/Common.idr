module ShellCompletion.Common

import Data.CompletionStyle
import Data.Config
import Data.Maybe
import ShellCompletion.Util
import Data.String

import System.Git

%default total

%hide Data.String.isPrefixOf

||| All available harmony subcommands (can be
||| specified as first argument to harmony).
export
allRootCmdsAndDescriptions : List (String, String)
allRootCmdsAndDescriptions = 
  [ ("request"   , "request review for the current branch (creating a PR if needed)")
  , ("rq"        , "request review for the current branch (creating a PR if needed)")
  , ("branch"    , "print a GitHub URI for the current branch")
  , ("config"    , "get or set configuration options")
  , ("contribute", "get one or more PRs awaiting your review")
  , ("graph"     , "graph data about recent PR review activity for a team")
  , ("health"    , "show open PR count by-month")
  , ("help"      , "show harmony usage and help")
  , ("label"     , "add one or more labels to a PR for the current branch, creating the PR if needed")
  , ("list"      , "list teams or list team members on a particular team")
  , ("pr"        , "get the current branch's PR or create one if needed")
  , ("reflect"   , "show detailed information about your own review activity")
  , ("sync"      , "pull labels, teams, etc. from GitHub and cache locally")
  , ("version"   , "show harmony version information")
  , ("whoami"    , "show information about the authenticated user")
  , ("quick"     , "create a new GitHub issue and branch quickly")
  ]

allRootCmds : (s : CompletionStyle) -> List CompletionResult
allRootCmds s = completionResult <$> allRootCmdsAndDescriptions

allQuickCmdOptsAndDescriptions : List (String, String)
allQuickCmdOptsAndDescriptions =
  [ ("--bugfix", "create a bugfix branch for the new issue")
  ]

allQuickCmdOpts : (s : CompletionStyle) -> List CompletionResult
allQuickCmdOpts s = completionResult <$> allQuickCmdOptsAndDescriptions

allPrCmdOptsAndDescriptions : List (String, String)
allPrCmdOptsAndDescriptions =
  [ ("--ready", "mark the new or existing PR as ready for review")
  , ("--draft", "mark the new or existing PR as a draft")
  , ("--into" , "set the branch to merge your PR into")
  ]

allPrCmdOpts : (s : CompletionStyle) -> List CompletionResult
allPrCmdOpts s = completionResult <$> allPrCmdOptsAndDescriptions

allContributeCmdOptsAndDescriptions : List (String, String)
allContributeCmdOptsAndDescriptions =
  [ ("--checkout", "check out the branch needing review")
  , ("-c"        , "check out the branch needing review")
  , ("--list"    , "list all branches needing review")
  , ("-l"        , "list all branches needing review")
  , ("--ignore"  , "ignore a given PR (just for the purposes of the contribute command)")
  , ("-i"        , "ignore a given PR (just for the purposes of the contribute command)")
  ]

allContributeCmdOpts : (s : CompletionStyle) -> List CompletionResult
allContributeCmdOpts s = completionResult <$> allContributeCmdOptsAndDescriptions

allGraphCmdOptsAndDescriptions : List (String, String)
allGraphCmdOptsAndDescriptions =
  [ ("--completed", "also graph completed PR reviews")
  , ("-c"         , "also graph completed PR reviews")
  ]

allGraphCmdOpts : (s : CompletionStyle) -> List CompletionResult
allGraphCmdOpts s = completionResult <$> allGraphCmdOptsAndDescriptions

allRequestCmdOptsAndDescriptions : List (String, String)
allRequestCmdOptsAndDescriptions =
  [ ("--dry", "do not actually request review")
  ]

allRequestCmdOpts : (s : CompletionStyle) -> List CompletionResult
allRequestCmdOpts s = completionResult <$> allRequestCmdOptsAndDescriptions

allSettableProps : (s : CompletionStyle) -> List CompletionResult
allSettableProps s = completionResult <$> settablePropNamesAndHelp
 
||| Attempt to handle completions for root commands but
||| if we ar not currently on the root command (at least
||| one argument has already been entered), we return
||| @Nothing@ so that code can call out to the full @opts@
||| function after loading the config file.
|||
||| Depending on the CompletionStyle, the return value will either be just a name or a "name:descriptuion"
export
cmdOpts : (s : CompletionStyle)
       -> (subcommand : String)
       -> (curWord : String)
       -> (prevWord : String)
       -> Maybe (List String)
-- first the root commands:
cmdOpts s _ "--"       "harmony" = all (allRootCmds s)
cmdOpts s _ partialCmd "harmony" = someWithPrefix partialCmd (allRootCmds s)

-- then the root commands that take no arguments.
-- this just stops autocomplete from populating any suggestions for these
-- commands' arguments.
cmdOpts _ "sync"    _ _ = Just []
cmdOpts _ "health"  _ _ = Just []
cmdOpts _ "--help"  _ _ = Just []
cmdOpts _ "reflect" _ _ = Just []
cmdOpts _ "version" _ _ = Just []

-- next subcommands that have options with no configuration requirement:
cmdOpts s "help" "--"       "help" = all (allRootCmds s)
cmdOpts s "help" partialArg "help" = someWithPrefix partialArg (allRootCmds s)

cmdOpts s "quick" "--"       "quick" = all (allQuickCmdOpts s)
cmdOpts s "quick" "-"        "quick" = someWithPrefix "--" (allQuickCmdOpts s)
cmdOpts s "quick" partialArg "quick" = someWithPrefix partialArg (allQuickCmdOpts s)

cmdOpts s "pr" "--"          "pr"      = all (allPrCmdOpts s)
cmdOpts s "pr" "-"           "pr"      = someWithPrefix "--" (allPrCmdOpts s)
cmdOpts _ "pr" partialBranch "--into"  = Nothing -- <- falls through to handle with config below.
cmdOpts s "pr" _             "--ready" = someFrom ["--into"] (allPrCmdOpts s) -- The ready flag does not work with the --draft flag.
cmdOpts s "pr" partialArg    "pr"      = 
  someWithPrefixOrNothing partialArg (allPrCmdOpts s) <|> 
    if isHashPrefix partialArg 
        then Nothing -- <- falls through to handle with config below.
        else Just []
cmdOpts s "pr" partialArg "--draft" =
  someWithPrefixOrNothing partialArg (filter (matches "--into") (allPrCmdOpts s)) <|>
    if isHashPrefix partialArg
       then Nothing -- <- falls through to handle with config below.
       else Just [] 
cmdOpts s "pr" partialArg branchName =
  -- we ignore the branch name, but this means --into has been used and we can
  -- avoid recommending it
  someWithPrefixOrNothing partialArg (filter (matches "--draft") (allPrCmdOpts s)) <|>
    if isHashPrefix partialArg
       then Nothing -- <- falls through to handle with config below.
       else Just []

cmdOpts s "contribute" "--"       _ = all (allContributeCmdOpts s) 
cmdOpts s "contribute" partialArg _ = someWithPrefix partialArg (allContributeCmdOpts s)

cmdOpts _ "graph" "--" _ = Nothing -- <- falls through to handle with config below.
cmdOpts s "graph" "-"  _ = all (allGraphCmdOpts s)
cmdOpts s "graph" partialArg _ =
  someWithPrefixOrNothing partialArg (allGraphCmdOpts s)

cmdOpts s "config" "--"        "config" = all (allSettableProps s)
cmdOpts s "config" partialProp "config" = someWithPrefix partialProp (allSettableProps s)
cmdOpts _ "config" _           _        = Just []

-- anything else requires configuration being loaded
cmdOpts _ _ _ _ = Nothing

optsForPrIntoOption : HasIO io => (partialBranch : String) -> io (Maybe (List String))
optsForPrIntoOption partialBranch = do
  allBranches <- listBranches'
  let matches = case partialBranch of
                     "--" => allBranches
                     _ => List.filter (isPrefixOf partialBranch) allBranches
  pure . Just $ matches

export
ffiOpts : HasIO io => (s : CompletionStyle) -> (subcommand : String) -> (curWord : String) -> (prevWord : String) -> io (Maybe (List String))
-- pr command (handled partially above, but when head references are specified, handled here)
-- we intentionally just return names without descriptions here for the
-- branches that match the partial argument
ffiOpts _ "pr" partialBranch "-i"     = optsForPrIntoOption partialBranch
ffiOpts _ "pr" partialBranch "--into" = optsForPrIntoOption partialBranch
ffiOpts _ _ _ _ = pure Nothing

optsForRequestCmd : Config => (s : CompletionStyle) -> String -> List String
optsForRequestCmd @{config} s partialArg =
  withPrefix partialArg (allRequestCmdOpts s) <|>
    slugsOrLoginsOrLabels
  where
    -- If the word being typed is prefixed with '+' return user logins
    -- but otherwise return team slugs. 
    slugsOrLoginsOrLabels : List String
    slugsOrLoginsOrLabels =
      case (asList partialArg) of
           ('+' :: _) => (strCons '+') <$> config.orgMembers
           _ => if isHashPrefix partialArg
                   then hashify . slugify <$> config.repoLabels
                   else config.teamSlugs

export
opts : Config => (s : CompletionStyle) -> (subcommand : String) -> (curWord : String) -> (prevWord : String) -> List String
-- we assume we are not handling a root command (see @cmdOpts@ which
-- should have already been called).

-- and the label command
opts @{config} _ "label" "--"         _ = 
  let labels := describe "\{config.repo} label" . slugify <$> config.repoLabels
  in  name <$> labels
opts @{config} _ "label" partialLabel _ = 
  let labels := describe "\{config.repo} label" . slugify <$> config.repoLabels
      filteredLabels := filter (isPrefixOf partialLabel) labels
  in  name <$> filteredLabels

-- then list, which only accepts a single team slug:
opts @{config} _ "list" "--"            "list" = 
  let teams := describe "\{config.repo} team" <$> config.teamSlugs
  in  name <$> teams
opts @{config} _ "list" partialTeamName "list" = 
  let teams := describe "\{config.repo} team" <$> config.teamSlugs
      filteredTeams := filter (isPrefixOf partialTeamName) teams
  in  name <$> filteredTeams
opts @{config} _ "list" "--" _ = []

-- then graph, which only accepts a single team slug
opts @{config} _ "graph" "--"            "graph"       = "--completed" :: config.teamSlugs
opts @{config} _ "graph" "--"            "--completed" = config.teamSlugs
opts @{config} _ "graph" partialTeamName previous =
  if isJust $ find (== previous) ["--completed", "graph"]
     then name <$> (filter (isPrefixOf partialTeamName) $ describe "\{config.repo} team" <$> config.teamSlugs)
     else []

-- then pr (handled partially above, but when labels are specified, handled here)
opts @{config} _ "pr" partialArg _ =
  if isHashPrefix partialArg
     then hashify . slugify <$> config.repoLabels
     else []

-- finally, request auto-completes with 
-- either a team slug or '+' followed by a user login:
opts @{config} _ "rq"      "--"       "rq"      = "--dry" :: config.teamSlugs
opts @{config} _ "request" "--"       "request" = "--dry" :: config.teamSlugs
opts @{config} _ "rq"      "--"       _         = config.teamSlugs
opts @{config} _ "request" "--"       _         = config.teamSlugs
opts           s "rq"      partialArg _         = optsForRequestCmd s partialArg
opts           s "request" partialArg _         = optsForRequestCmd s partialArg

opts _ _ _ _ = []
