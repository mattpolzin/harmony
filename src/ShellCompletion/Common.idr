module ShellCompletion.Common

import Data.CompletionStyle
import Data.Config
import Data.Issue
import Data.Maybe
import Data.Project
import Data.Promise
import Data.String

import CommandStubs
import Util.ShellCompletion
import Util.String

import FFI.GitHub

import System.Git

%default total

isStrPrefixOf : String -> String -> Bool
isStrPrefixOf = Data.String.isPrefixOf

%hide Data.String.isPrefixOf

||| All available harmony subcommands (can be
||| specified as first argument to harmony).
allRootCmdsAndDescriptions : List (String, String)
allRootCmdsAndDescriptions = map unpack allCommands
  where
    unpack : Command -> (String, String)
    unpack (C name shortDescription) = (name, shortDescription)

allRootCmds : (s : CompletionStyle) -> List CompletionResult
allRootCmds s = completionResult <$> allRootCmdsAndDescriptions

allVersionOptsAndDescriptions : List (String, String)
allVersionOptsAndDescriptions =
  [ ("-s"     , "print a shorter version more useful to scripting application")
  , ("--short", "print a shorter version more useful to scripting application")
  ]

allVersionOpts : (s : CompletionStyle) -> List CompletionResult
allVersionOpts s = completionResult <$> allVersionOptsAndDescriptions

allQuickCmdOptsAndDescriptions : List (String, String)
allQuickCmdOptsAndDescriptions =
  [ ("--bugfix" , "create a bugfix branch for the new issue")
  , ("--project", "associate an existing project with the new issue")
  ]

allQuickCmdOpts : (s : CompletionStyle) -> List CompletionResult
allQuickCmdOpts s = completionResult <$> allQuickCmdOptsAndDescriptions

allSlowCmdOptsAndDescriptions : List (String, String)
allSlowCmdOptsAndDescriptions =
  [ ("-s"    , "stub out sub-issues")
  , ("--stub", "stub out sub-issues")
  ]

allSlowCmdOpts : (s : CompletionStyle) -> List CompletionResult
allSlowCmdOpts s = completionResult <$> allSlowCmdOptsAndDescriptions

data ProjectStyle = Number | Title | All

allProjectOptsAndDescriptions : Config -> ProjectStyle -> List (String, String)
allProjectOptsAndDescriptions config projectStyle =
  config.repoProjects >>= numAndTitle
  
  where
    projectNumberOpt : ProjectRef -> Maybe (String, String)
    projectNumberOpt proj =
      case projectStyle of
         Title => Nothing
         _     => Just (show proj.number, proj.title)

    projectTitleOpt : ProjectRef -> Maybe (String, String)
    projectTitleOpt proj =
      case projectStyle of
         Number => Nothing
         _     => Just (slugify proj.title, "\{config.repo} project")

    numAndTitle : ProjectRef -> List (String, String)
    numAndTitle proj = catMaybes [ projectTitleOpt proj, projectNumberOpt proj ]

allProjectOpts : (s : CompletionStyle) -> Config -> ProjectStyle -> List CompletionResult
allProjectOpts s = map completionResult .: allProjectOptsAndDescriptions

allPrCmdOptsAndDescriptions : List (String, String)
allPrCmdOptsAndDescriptions =
  [ ("--ready"     , "mark the new or existing PR as ready for review")
  , ("--draft"     , "mark the new or existing PR as a draft")
  , ("--issue"     , "create and link a GitHub Issue")
  , ("--project"   , "associate an existing project with the new issue (only supported when --issue is used)")
  , ("--print-tree", "print a tree of PRs between the current branch and the main branch")
  , ("--output"    , "output in the given format (at least for non-error responses)")
  , ("--into"      , "set the branch to merge your PR into")
  ]

allPrCmdOpts : (s : CompletionStyle) -> List CompletionResult
allPrCmdOpts s = completionResult <$> allPrCmdOptsAndDescriptions

allOutputFormatOptsAndDescriptions : List (String, String)
allOutputFormatOptsAndDescriptions =
  [ ("shell"     , "[default] output for the shell (with colors if supported)")
  , ("markdown"  , "output in markdown")
  ]

allOutputFormatOpts : (s : CompletionStyle) -> List CompletionResult
allOutputFormatOpts s = completionResult <$> allOutputFormatOptsAndDescriptions

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

allSettableValuesForProp : (s : CompletionStyle) -> (propName : String) -> List CompletionResult
allSettableValuesForProp s n with (settablePropNamed n)
  _ | Nothing = []
  _ | Just p = describe "" <$> (propOptions $ snd $ reifyProp p)
 
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

-- next subcommands that have options with no configuration requirement:
cmdOpts s "version" "--"       "version" = all (allVersionOpts s)
cmdOpts s "version" partialArg "version" = someWithPrefix partialArg (allVersionOpts s)

cmdOpts s "help" "--"       "help" = all (allRootCmds s)
cmdOpts s "help" partialArg "help" = someWithPrefix partialArg (allRootCmds s)

cmdOpts s "quick" "--"       "quick"     = all (allQuickCmdOpts s)
cmdOpts s "quick" "-"        "quick"     = someWithPrefix "--" (allQuickCmdOpts s)
cmdOpts s "quick" _          "--project" = Nothing -- <- fall through to handle project autocompletion
cmdOpts s "quick" partialArg "quick"     =
  if isHashPrefix partialArg
     then Nothing -- <- falls through to handle with config below.
     else someWithPrefix partialArg (allQuickCmdOpts s)

cmdOpts s "slow" "--" "slow"   = Nothing
cmdOpts s "slow" "-"  "slow"   = someWithPrefix "--" (allSlowCmdOpts s)
cmdOpts s "slow" _    "--stub" = Nothing -- <- fall through to handle issue autocompletion
cmdOpts s "slow" _    "-s"     = Nothing -- <- fall through to handle issue autocompletion
cmdOpts s "slow" partialArg _  = Nothing -- <- fall through to handle issue autocompletion

cmdOpts s "pr" "--"          "pr"        = all (allPrCmdOpts s)
cmdOpts s "pr" "-"           "pr"        = someWithPrefix "--" (allPrCmdOpts s)
cmdOpts _ "pr" partialBranch "--project" = Nothing -- <- falls through to handle with config below.
cmdOpts _ "pr" partialBranch "--into"    = Nothing -- <- falls through to handle with config below.
cmdOpts s "pr" "--"          "--output"  = all (allOutputFormatOpts s)
cmdOpts s "pr" partialFormat "--output"  = someWithPrefix partialFormat (allOutputFormatOpts s)
cmdOpts s "pr" "--"          "-o"        = all (allOutputFormatOpts s)
cmdOpts s "pr" partialFormat "-o"        = someWithPrefix partialFormat (allOutputFormatOpts s)
cmdOpts s "pr" _             "--ready"   = someFrom ["--issue", "--print-tree", "--into", "--output"] (allPrCmdOpts s) -- The ready flag does not work with the --draft flag.
cmdOpts s "pr" partialArg    "pr" = 
  someWithPrefixOrNothing partialArg (allPrCmdOpts s) <|> 
    if isHashPrefix partialArg 
        then Nothing -- <- falls through to handle with config below.
        else Just []
cmdOpts s "pr" partialArg "--draft" =
  someWithPrefixOrNothing partialArg (filter (\c => matches "--output" c || matches "--into" c || matches "--issue" c || matches "--print-tree" c) (allPrCmdOpts s)) <|>
    if isHashPrefix partialArg
       then Nothing -- <- falls through to handle with config below.
       else Just [] 
cmdOpts s "pr" partialArg "--issue" =
  someWithPrefixOrNothing partialArg (filter (\c => matches "--output" c || matches "--into" c || matches "--ready" c || matches "--draft" c || matches "--print-tree" c) (allPrCmdOpts s)) <|>
    if isHashPrefix partialArg
       then Nothing -- <- falls through to handle with config below.
       else Just []
cmdOpts s "pr" partialArg lastArg =
  let prefixMatch = 
    if isJust $ List.find (== lastArg) (fst <$> allOutputFormatOptsAndDescriptions)
       then -- we ignore the last arg, but this means we can avoid recommending the output option
            someWithPrefixOrNothing
              partialArg
              (filter (\c => doesNotMatch "--output" c) (allPrCmdOpts s))
       else -- we ignore the branch name, but this means --into has been used
            -- and the --ready flag is not appropriate because creating a new
            -- PR is implied by --into and the --ready flag is only used on existing
            -- draft PRs. We can avoid recommending them.
            someWithPrefixOrNothing 
              partialArg 
              (filter (\c => doesNotMatch "--into" c && doesNotMatch "--ready" c) (allPrCmdOpts s))
   in prefixMatch <|>
        if isHashPrefix partialArg
           then Nothing -- <- falls through to handle with config below.
           else Just []
cmdOpts s "contribute" "--"       _ = all (allContributeCmdOpts s) 
cmdOpts s "contribute" partialArg _ = someWithPrefix partialArg (allContributeCmdOpts s)

cmdOpts _ "graph" "--" _ = Nothing -- <- falls through to handle with config below.
cmdOpts s "graph" "-"  _ = all (allGraphCmdOpts s)
cmdOpts s "graph" partialArg _ =
  someWithPrefixOrNothing partialArg (allGraphCmdOpts s)

cmdOpts s "config" "--"         "config" = all (allSettableProps s)
cmdOpts s "config" partialProp  "config" = someWithPrefix partialProp (allSettableProps s)
cmdOpts s "config" _            "defaultProject" = Nothing -- <- falls through
  -- ^  defaultProject is special cased because its options come from the
  -- config cache of projects for the repo.
cmdOpts s "config" "--"         settableProp = all (allSettableValuesForProp s settableProp)
cmdOpts s "config" partialValue settableProp =
  someWithPrefix partialValue (allSettableValuesForProp s settableProp)

-- anything else requires configuration being loaded, FFI, or GitHub requests
cmdOpts _ _ _ _ = Nothing

optsForPrIntoOption : HasIO io => (partialBranch : String) -> io (Maybe (List String))
optsForPrIntoOption partialBranch = do
  allBranches <- listBranches'
  let matches = case partialBranch of
                     "--" => allBranches
                     _ => List.filter (isStrPrefixOf partialBranch) allBranches
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
|||
||| In many cases, the config file is more of a cache file used by completion
||| to populate results without needing to make HTTP requests just to offer up
||| suggestions at the CLI.
export
configuredOpts : Config => 
       (s : CompletionStyle)
    -> (subcommand : String)
    -> (curWord : String)
    -> (prevWord : String)
    -> List String
-- we assume we are not handling a root command (see @cmdOpts@ which
-- should have already been called).

-- the config command
configuredOpts @{config} s "config" "--" "defaultProject" =
  stringify' (allProjectOpts s config Number)
configuredOpts @{config} s "config" partialProjectRef "defaultProject" =
  withPrefix partialProjectRef (allProjectOpts s config Number)

-- the quick command
configuredOpts @{config} s "quick" "--" "--project" = 
  stringify' (allProjectOpts s config Title)
configuredOpts @{config} s "quick" partialProjectRef "--project" =
  withPrefix partialProjectRef (allProjectOpts s config All)

-- the label command
configuredOpts @{config} _ "label" "--"         _ = 
  let labels := describe "\{config.repo} label" . slugify <$> config.repoLabels
  in  name <$> labels
configuredOpts @{config} _ "label" partialLabel _ = 
  let labels := describe "\{config.repo} label" . slugify <$> config.repoLabels
      filteredLabels := filter (isPrefixOf partialLabel) labels
  in  name <$> filteredLabels

-- list, which only accepts a single team slug:
configuredOpts @{config} _ "list" "--"            "list" = 
  let teams := describe "\{config.repo} team" <$> config.teamSlugs
  in  name <$> teams
configuredOpts @{config} _ "list" partialTeamName "list" = 
  let teams := describe "\{config.repo} team" <$> config.teamSlugs
      filteredTeams := filter (isPrefixOf partialTeamName) teams
  in  name <$> filteredTeams
configuredOpts @{config} _ "list" "--" _ = []

-- graph, which only accepts a single team slug
configuredOpts @{config} _ "graph" "--"            "graph"       = "--completed" :: config.teamSlugs
configuredOpts @{config} _ "graph" "--"            "--completed" = config.teamSlugs
configuredOpts @{config} _ "graph" partialTeamName previous =
  if isJust $ find (== previous) ["--completed", "graph"]
     then name <$> (filter (isPrefixOf partialTeamName) $ describe "\{config.repo} team" <$> config.teamSlugs)
     else []

-- pr (handled partially above, but when a project or labels are specified, handled here)
configuredOpts @{config} s "pr" "--" "--project" = 
  stringify' (allProjectOpts s config Title)
configuredOpts @{config} s "pr" partialProjectRef "--project" =
  withPrefix partialProjectRef (allProjectOpts s config All)
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
  in  if substr `isStrPrefixOf` numStr
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
compareIssues u (MkIssue _ _ _ _ _ _ assignee1 (Just prCount1)) (MkIssue _ _ _ _ _ _ assignee2 (Just prCount2)) =
  case compare prCount1 prCount2 of
       LT => LT
       GT => GT
       EQ => compareAssignees u assignee1 assignee2
compareIssues u (MkIssue _ _ _ _ _ _ assignee1 Nothing) (MkIssue _ _ _ _ _ _ assignee2 (Just _)) =
  LT
compareIssues u (MkIssue _ _ _ _ _ _ assignee1 (Just _)) (MkIssue _ _ _ _ _ _ assignee2 Nothing) =
  GT
compareIssues u (MkIssue _ _ _ _ _ _ assignee1 Nothing) (MkIssue _ _ _ _ _ _ assignee2 Nothing) =
  compareAssignees u assignee1 assignee2

issueByNum : (partialArg : String) ->  Issue -> Maybe (String, Issue)
issueByNum partialArg i = (, i) <$> hashifyIfPrefix partialArg i.number

issueByTitle : (partialArg : String) -> Issue -> Maybe (String, Issue)
issueByTitle partialArg i =
  if partialArg `isStrPrefixOf` i.title
     then Just (slugify i.title, i)
     else Nothing

||| Get issue completions matching the partial arg for issue title.
issuesByTitle : (partialArg : String) -> List Issue -> List (String, Issue)
issuesByTitle partialArg issues =
  catMaybes $ map (issueByTitle partialArg) issues

||| Get issue completions matching the partial arg for issue number.
issuesByNum : (partialArg : String) -> List Issue -> List (String, Issue)
issuesByNum partialArg issues =
  catMaybes $ map (issueByNum partialArg) issues

||| Get issue completions matching the partial arg for either issue number or
||| issue title.
|||
||| IMPORTANT: The partial arg for an issue number should have its '#' stripped
||| off (unhashified).
issuesByNumAndTitle : (partialArg : String) -> List Issue -> List (String, Issue)
issuesByNumAndTitle partialArg issues =
  issues >>= (\i => catMaybes [byNum i, byTitle i])

  where
    byNum : Issue -> Maybe (String, Issue)
    byNum = issueByNum partialArg

    byTitle : Issue -> Maybe (String, Issue)
    byTitle = issueByTitle partialArg

strCompletion : CompletionStyle => (String, String) -> String
strCompletion = stringify . completionResult

parameters (githubUser : Maybe String)
  issueSorter : (a, Issue) -> (a, Issue) -> Ordering
  issueSorter = (compareIssues githubUser) `on` snd

  issueDescriber : Issue -> String
  issueDescriber = describe githubUser

  issueOpts : CompletionStyle => List (String, Issue) -> List String
  issueOpts issues = strCompletion . mapSnd issueDescriber <$> sortBy issueSorter issues

||| Opts for which completion requires reaching out to GitHub.
export
githubOpts : Config =>
             Lazy Octokit
          -> (s : CompletionStyle)
          -> (subcommand : String)
          -> (curWord : String)
          -> (prevWord : String)
          -> Promise' (List String)
githubOpts @{config} gh _ "quick" partialArg _ = do
  issues <- listIssues @{gh} config.org config.repo 30
  let partialArg' = unhashify partialArg
  let issues' =
    mapMaybe (\i => (, i) <$> hashifyIfPrefix partialArg' i.number) issues
  pure (issueOpts config.githubUser issues')
githubOpts @{config} gh s "slow" partialArg _ = do
  issues <- listIssues @{gh} config.org config.repo 30
  let partialArg' = if partialArg == "--"
                       then ""
                       else unhashify partialArg
  let regularOpts = maybe [] id $ someWithPrefix partialArg (allSlowCmdOpts s)
  let issues' = if partialArg' == ""
                   then issuesByNum "" issues
                   else issuesByNumAndTitle partialArg' issues
  pure (regularOpts ++ (issueOpts config.githubUser issues'))
githubOpts _ _ _ _ _ = pure []
