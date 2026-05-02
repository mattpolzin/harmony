module ShellCompletion.Common

import Data.CompletionStyle
import Data.Config
import Data.Issue
import Data.Maybe
import Data.Promise
import Data.String

import ShellCompletion.Util
import Util

import FFI.GitHub

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
  [ ("--ready"     , "mark the new or existing PR as ready for review")
  , ("--draft"     , "mark the new or existing PR as a draft")
  , ("--print-tree", "print a tree of PRs between the current branch and the main branch")
  , ("--into"      , "set the branch to merge your PR into")
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
||| Depending on the CompletionStyle, the return value will either be just a
||| name or a "name:descriptuion"
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
cmdOpts s "quick" partialArg "quick" = 
  if isHashPrefix partialArg
     then Nothing -- <- falls through to handle with config below.
     else someWithPrefix partialArg (allQuickCmdOpts s)

cmdOpts s "pr" "--"          "pr"      = all (allPrCmdOpts s)
cmdOpts s "pr" "-"           "pr"      = someWithPrefix "--" (allPrCmdOpts s)
cmdOpts _ "pr" partialBranch "--into"  = Nothing -- <- falls through to handle with config below.
cmdOpts s "pr" _             "--ready" = someFrom ["--print-tree", "--into"] (allPrCmdOpts s) -- The ready flag does not work with the --draft flag.
cmdOpts s "pr" partialArg    "pr"      = 
  someWithPrefixOrNothing partialArg (allPrCmdOpts s) <|> 
    if isHashPrefix partialArg 
        then Nothing -- <- falls through to handle with config below.
        else Just []
cmdOpts s "pr" partialArg "--draft" =
  someWithPrefixOrNothing partialArg (filter (\c => matches "--into" c || matches "--print-tree" c) (allPrCmdOpts s)) <|>
    if isHashPrefix partialArg
       then Nothing -- <- falls through to handle with config below.
       else Just [] 
cmdOpts s "pr" partialArg branchName =
  -- we ignore the branch name, but this means --into has been used and we can
  -- avoid recommending it
  someWithPrefixOrNothing partialArg (filter (\c => matches "--draft" c || matches "--print-tree" c) (allPrCmdOpts s)) <|>
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

||| Opts for which completion requires IO but not harmony configuration.
export
ffiOpts : HasIO io => (s : CompletionStyle) -> (subcommand : String) -> (curWord : String) -> (prevWord : String) -> io (Maybe (List String))
-- pr command (handled partially above, but when head references are specified,
-- handled here) we intentionally just return names without descriptions here
-- for the branches that match the partial argument
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

||| opts for which completion requires full harmony configuration.
export
configuredOpts : Config => 
       (s : CompletionStyle)
    -> (subcommand : String)
    -> (curWord : String)
    -> (prevWord : String)
    -> List String
-- we assume we are not handling a root command (see @cmdOpts@ which
-- should have already been called).

-- and the label command
configuredOpts @{config} _ "label" "--"         _ = 
  let labels := describe "\{config.repo} label" . slugify <$> config.repoLabels
  in  name <$> labels
configuredOpts @{config} _ "label" partialLabel _ = 
  let labels := describe "\{config.repo} label" . slugify <$> config.repoLabels
      filteredLabels := filter (isPrefixOf partialLabel) labels
  in  name <$> filteredLabels

-- then list, which only accepts a single team slug:
configuredOpts @{config} _ "list" "--"            "list" = 
  let teams := describe "\{config.repo} team" <$> config.teamSlugs
  in  name <$> teams
configuredOpts @{config} _ "list" partialTeamName "list" = 
  let teams := describe "\{config.repo} team" <$> config.teamSlugs
      filteredTeams := filter (isPrefixOf partialTeamName) teams
  in  name <$> filteredTeams
configuredOpts @{config} _ "list" "--" _ = []

-- then graph, which only accepts a single team slug
configuredOpts @{config} _ "graph" "--"            "graph"       = "--completed" :: config.teamSlugs
configuredOpts @{config} _ "graph" "--"            "--completed" = config.teamSlugs
configuredOpts @{config} _ "graph" partialTeamName previous =
  if isJust $ find (== previous) ["--completed", "graph"]
     then name <$> (filter (isPrefixOf partialTeamName) $ describe "\{config.repo} team" <$> config.teamSlugs)
     else []

-- then pr (handled partially above, but when labels are specified, handled here)
configuredOpts @{config} _ "pr" partialArg _ =
  if isHashPrefix partialArg
     then hashify . slugify <$> config.repoLabels
     else []

-- finally, request auto-completes with 
-- either a team slug or '+' followed by a user login:
configuredOpts @{config} _ "rq"      "--"       "rq"      = "--dry" :: config.teamSlugs
configuredOpts @{config} _ "request" "--"       "request" = "--dry" :: config.teamSlugs
configuredOpts @{config} _ "rq"      "--"       _         = config.teamSlugs
configuredOpts @{config} _ "request" "--"       _         = config.teamSlugs
configuredOpts           s "rq"      partialArg _         = optsForRequestCmd s partialArg
configuredOpts           s "request" partialArg _         = optsForRequestCmd s partialArg

configuredOpts _ _ _ _ = []

hashifyIfPrefix : (substr : String) -> (issueNumber : Integer) -> Maybe String
hashifyIfPrefix substr num =
  let numStr := show num
  in  if substr `isPrefixOf` numStr
         then Just . hashify $ numStr
         else Nothing

describe : (githubUser : Maybe String) -> Issue -> String
describe user issue = "\{assigned user}\{openPrs}\{issue.title}"
  where
    openPrs : String
    openPrs = case issue.linkedPRCount of
                   Just 0  => ""
                   Just 1  => "{1 PR} "
                   Just n  => "{\{show n} PRs} "
                   Nothing => ""

    assigned : Maybe String -> String
    assigned Nothing           = ""
    assigned (Just githubUser) =
      case issue.assignee of
           Nothing => ""
           Just assignee =>
             if assignee == githubUser
                then "{yours} "
                else "{taken} "

||| Put issues assigned to the given user at the top and issues assigned to
||| other users at the bottom with everything else between.
compareAssignees : (githubUser : Maybe String) -> (assignee1 : Maybe String) -> (assignee2 : Maybe String) -> Ordering
compareAssignees Nothing _ _ = EQ
compareAssignees _ Nothing Nothing = EQ
compareAssignees (Just u) Nothing (Just a2) =
  if u == a2 then GT else LT
compareAssignees (Just u) (Just a1) Nothing =
  if u == a1 then LT else GT
compareAssignees (Just u) (Just a1) (Just a2) =
  if a1 == a2
     then EQ
     else if u == a2
             then GT
             else if u == a1
                     then LT
                     else EQ

compareIssues : (githubUser : Maybe String) -> Issue -> Issue -> Ordering
compareIssues u (MkIssue _ _ _ _ _ assignee1 (Just prCount1)) (MkIssue _ _ _ _ _ assignee2 (Just prCount2)) =
  case compare prCount1 prCount2 of
       LT => LT
       GT => GT
       EQ => compareAssignees u assignee1 assignee2
compareIssues u (MkIssue _ _ _ _ _ assignee1 Nothing) (MkIssue _ _ _ _ _ assignee2 (Just _)) =
  LT
compareIssues u (MkIssue _ _ _ _ _ assignee1 (Just _)) (MkIssue _ _ _ _ _ assignee2 Nothing) =
  GT
compareIssues u (MkIssue _ _ _ _ _ assignee1 Nothing) (MkIssue _ _ _ _ _ assignee2 Nothing) =
  compareAssignees u assignee1 assignee2

export
githubOpts : Config =>
             Lazy Octokit
          -> (s : CompletionStyle)
          -> (subcommand : String)
          -> (curWord : String)
          -> (prevWord : String)
          -> Promise' (List String)
githubOpts @{config} gh _ "quick" partialArg _ = do
  issues <- listIssues @{gh} config.org config.repo
  let partialArg' = unhashify partialArg
  let str = stringify . completionResult
  let sorter = (compareIssues config.githubUser) `on` snd
  let describer = describe config.githubUser
  let issues' =
    mapMaybe (\i => (, i) <$> hashifyIfPrefix partialArg' i.number)
             issues
  pure (str . mapSnd describer <$> sortBy sorter issues')
githubOpts _ _ _ _ _ = pure []

