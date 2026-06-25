module Commands

import Commands.Graph
import Commands.Label
import Commands.PullRequest
import public Commands.Quick
import Commands.Reviewer
import Commands.User

import Data.Config
import Data.DPair
import Data.Date
import Data.Either
import Data.Fuel
import Data.Issue
import Data.List
import Data.List1
import Data.Promise
import Data.PullRequest
import Data.SortedMap
import Data.String
import Data.User

import ShellCompletion.Util
import Config
import FFI.Concurrency
import FFI.GitHub
import Util
import System.Git

import JSON.Parser
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

||| Sync the cached data in the config file (used for auto-completion, mostly).
export
sync : Config => Octokit =>
       Promise' ()
sync = ignore $ syncConfig True

export
printConfig : Config => Promise' ()
printConfig @{config} = do
  renderIO $
    vsep [ "USAGE: harmony config [property] [value]"
         , ""
         , "Specify a property to read it out or specify both a property and a value to set it to."
         , ""
         , annotate underline "Settable Properties:"
         , settablePropsWithHelp
         , ""
         ]
  if config.ttyStdout
     then do waitForEnter "print the current configuration"
             putStrLn ""
     else pure ()
  renderIO $
    vsep [ annotate underline "Current Configuration:"
         , render config
         ]

||| Provide information about who the current user is when
||| they execute `harmony whoami`.
export
whoami : Config => Octokit =>
         Promise' ()
whoami = printInfoOnSelf

||| Provide information on the curent user's recent work and currrent
||| workflow when they execute `harmony relfect`.
export
reflect : Config => Octokit =>
          Promise' ()
reflect = reflectOnSelf

||| Apply the given labels to the current PR when the user executes
||| `harmony label <label> ...`.
export
label : Config => Octokit =>
        (labels : List String)
     -> Promise' ()
label @{config} labels =
  do Actual _ openPr <- identifyOrCreatePR !currentBranch
       | Hypothetical _ => reject "You cannot label a PR that has not been created yet and you cannot create the PR in this non-TTY shell"
     let finalLabels = unslugifyLabel config.repoLabels <$> labels
     allLabels <- addLabels openPr finalLabels
     renderIO $ vsep
       [ "Added" <++> putLabels finalLabels <+> " to PR."
       , pretty "All labels for PR of \{openPr.headRef}:" <++> putLabels allLabels <+> "."
       ]
  where
    putLabel : String -> Doc AnsiStyle
    putLabel = enclose "\"" "\"" . annotate (color Green) . pretty

    putLabels : List String -> Doc AnsiStyle
    putLabels = hcat . intersperse (pretty ", ") . map putLabel

prUsageError  : String
prUsageError = 
  "pr's arguments must be #<label>, --into <branch-name>, --ready, --draft, or --issue."

data IntoOpt = Branch (Exists String.NonEmpty)

data OutputFormat = Shell | Markdown

data PrArg = Draft | Ready | CreateIssue | PrintTree | Output OutputFormat | Into IntoOpt | Label String

(<||>) : Alternative t => (a -> t b) -> (a -> t b) -> a -> t b
(<||>) f g x = f x <|> g x

private infixr 2 <||>

||| Parse arguments to the pr subcommand.
export
parsePrArgs : List String -> Either String (List PrArg)
parsePrArgs [] = Right []
parsePrArgs args =
  let (intoArgs, rest) = recombineIntoArgs args
      intoArgs' = Into <$> intoArgs
      (outputArgs, rest') = recombineOutputArgs rest
      outputArgs' = Output <$> outputArgs
      rest'' = (traverse (parseReadyFlag <||> parseDraftFlag <||> parseIssueFlag <||> parsePrintTreeFlag <||> parseLabelArg) rest')
      in  maybeToEither prUsageError ((intoArgs' ++ outputArgs' ++) <$> rest'')
  where
    parseDraftFlag : String -> Maybe PrArg
    parseDraftFlag "--draft" = Just Draft
    parseDraftFlag _ = Nothing

    parseReadyFlag : String -> Maybe PrArg
    parseReadyFlag "--ready" = Just Ready
    parseReadyFlag _ = Nothing

    parseIssueFlag : String -> Maybe PrArg
    parseIssueFlag "--issue" = Just CreateIssue
    parseIssueFlag _ = Nothing

    parsePrintTreeFlag : String -> Maybe PrArg
    parsePrintTreeFlag "--print-tree" = Just PrintTree
    parsePrintTreeFlag _ = Nothing

    parseLabelArg : String -> Maybe PrArg
    parseLabelArg labelArg =
      case strM labelArg of
           (StrCons '#' label) => Just $ Label label
           _                   => Nothing

    -- expect a String
    parseIntoOpt : String -> Maybe IntoOpt
    parseIntoOpt str =
      let str' = nonEmpty str
      in Branch . Evidence str <$> str'

    -- the --into option takes the next argument as its input so we will
    -- take two consecutive list elements and combine them for that option.
    -- The options will be returned separately and the rest will be left for
    -- later.
    recombineIntoArgs : List String -> (List IntoOpt, List String)
    recombineIntoArgs [] = ([], [])
    recombineIntoArgs ("-i" :: []) = ([], ["-i"])
    recombineIntoArgs ("--into" :: []) = ([], ["--into"])
    recombineIntoArgs ("-i" :: (x :: xs)) =
      case parseIntoOpt x of
           Just opt => mapFst (opt ::) (recombineIntoArgs xs)
           Nothing  => mapSnd (\xs' => "-i" :: x :: xs') (recombineIntoArgs xs)
    recombineIntoArgs ("--into" :: (x :: xs)) =
      case parseIntoOpt x of
           Just opt => mapFst (opt ::) (recombineIntoArgs xs)
           Nothing  => mapSnd (\xs' => "--into" :: x :: xs') (recombineIntoArgs xs)
    recombineIntoArgs (x :: xs) = mapSnd (x ::) (recombineIntoArgs xs)

    parseOutputFormat : String -> Maybe OutputFormat
    parseOutputFormat "markdown" = Just Markdown
    parseOutputFormat "shell"    = Just Shell
    parseOutputFormat _ = Nothing

    -- the -o/--output option takes the next argument as its input so we will
    -- take two consecutive list elements and combine them for that option.
    -- The options will be returned separately and the rest will be left for
    -- later.
    recombineOutputArgs : List String -> (List OutputFormat, List String)
    recombineOutputArgs [] = ([], [])
    recombineOutputArgs ("-o" :: []) = ([], ["-o"])
    recombineOutputArgs ("--output" :: []) = ([], ["--output"])
    recombineOutputArgs ("-o" :: (x :: xs)) =
      case parseOutputFormat x of
           Just opt => mapFst (opt ::) (recombineOutputArgs xs)
           Nothing  => mapSnd (\xs' => "-o" :: x :: xs') (recombineOutputArgs xs)
    recombineOutputArgs ("--output" :: (x :: xs)) =
      case parseOutputFormat x of
           Just opt => mapFst (opt ::) (recombineOutputArgs xs)
           Nothing  => mapSnd (\xs' => "--output" :: x :: xs') (recombineOutputArgs xs)
    recombineOutputArgs (x :: xs) = mapSnd (x ::) (recombineOutputArgs xs)

||| Print the URI for the current branch's PR or create a new PR if one
||| does not exist when the user executes `harmony pr`. Supports creation
||| of draft PRs (default False) and can accept any number of labels to apply
||| to the new or current PR.
export
pr : Config => Octokit =>
     (args : List PrArg)
  -> Promise' ()
pr @{config} args = do
  when conflictingDraftReadyArgs $
    reject "You cannot set a PR as ready for review and mark it as a draft at the same time."
  Actual actionTaken pr <- identifyOrCreatePR {markAsDraft} {createIssueForPR} {intoBranch} !currentBranch
    | Hypothetical url => putStrLn url
  case actionTaken of
       Identified => if printTree then printPrTree pr else putStrLn pr.webURI
       Created    => if printTree then printPrTree pr else pure ()
  when (not $ null labelSlugs) $
    label labelSlugs
  whenJust intoBranch $ \branch =>
    if not (branch `isSuffixOf` pr.baseRef)
      then reject "Setting the --into branch (base ref) for an existing PR is not supported (yet). Base ref will remain \{pr.baseRef}"
      else pure ()
  when (markAsDraft && not pr.isDraft) $ do
    putStrLn ""
    True <- yesNoPrompt {defaultAnswer = False} "Are you sure you want to convert the existing PR for the current branch to a draft?"
      | False => putStrLn "No worries, the PR won't be converted to a draft."
    ignore $ convertPRToDraft pr
    putStrLn "The PR for the current branch has been converted to a draft."
  when (markAsReady && pr.isDraft) $ do
    putStrLn ""
    True <- yesNoPrompt {defaultAnswer = False} "Are you sure you want to mark the existing PR for the current branch ready for review?"
      | False => putStrLn "No worries, the PR won't be marked ready for review."
    ignore $ convertPRToReady pr
    putStrLn "The PR for the current branch has been marked ready for review."

  where
    markAsDraft : Bool
    markAsDraft = isJust $ find (\case Draft => True; _ => False) args

    markAsReady : Bool
    markAsReady = isJust $ find (\case Ready => True; _ => False) args

    createIssueForPR : Bool
    createIssueForPR = isJust $ find (\case CreateIssue => True; _ => False) args

    conflictingDraftReadyArgs : Bool
    conflictingDraftReadyArgs = markAsDraft && markAsReady

    labelSlugs : List String
    labelSlugs = foldr (\case (Label l) => (l ::); _ => id) [] args

    intoBranch : Maybe String
    intoBranch =
      foldMap (\case (Into (Branch name)) => Just (value name.snd); _ => Nothing) args

    printTree : Bool
    printTree = isJust $ find (\case PrintTree => True; _ => False) args

    firstFormat : PrArg -> Maybe RenderFormat -> Maybe RenderFormat
    firstFormat _                 f@(Just _) = f
    firstFormat (Output Shell)    _          = Just Shell
    firstFormat (Output Markdown) _          = Just Markdown
    firstFormat _                 y          = y

    renderFormat : RenderFormat
    renderFormat = maybe Shell id $ foldr firstFormat Nothing args

    printPrTree : PullRequest -> Promise' ()
    printPrTree pr = do
      (upstreamPrs, terminalBranch) <- upstreamPrChain (limit 10) pr.baseRef
      downstreamPrs <- downstreamPrChain (limit 10) pr.headRef
      let nodes = prTree Nothing Nothing downstreamPrs (pr :: upstreamPrs) terminalBranch
      putStrLn $ renderPrTree renderFormat nodes

||| Get Pull Requests that are upstream of the given
||| branch's PR. This is an internal-only command at the
||| moment.
||| Produces an object with a `prs` entry that is a JSON
||| array of PRs and a `terminalBranch` entry. See the
||| description of the
||| Commands.PullRequest.upstreamPrChain function for
||| details.
export
upstreamPrsJsonStr : Config => Octokit => Fuel -> (branch : String) -> Promise' String
upstreamPrsJsonStr fuel branch = do
  (prs, terminalBranch) <- upstreamPrChain fuel branch
  let json = JObject [ ("prs", JArray $ json <$> prs)
                     , ("terminalBranch", JString terminalBranch)
                     ]
  pure (show json)

||| Get Pull Requests that are downstream of the given
||| branch's PR. This is an internal-only command at the
||| moment.
|||
||| Produces a JSON array of PRs
export
downstreamPrsJsonStr : Config => Octokit => Fuel -> (branch : String) -> Promise' String
downstreamPrsJsonStr fuel branch = do
  prs <- downstreamPrChain fuel branch
  let json = JArray $ json <$> prs
  pure (show json)

||| Request review from the given teams & users as reviewers when the user executes
||| `harmony request ...`.
export
request : Config => Octokit => 
         (requestArgs : List String) 
      -> {default False dry : Bool} 
      -> Promise' ()
request args {dry} = do
  let (forcedReviewers, teamNames, labelSlugs) = partitionedArgs
  if (null forcedReviewers && null teamNames)
     then reject "The request command expects one or more names of GitHub Teams or Users as arguments."
     else do Actual _ openPr <- identifyOrCreatePR !currentBranch
               | Hypothetical _ => reject "You cannot request review on a PR that has not been created yet and you cannot create the PR in this non-TTY shell"
             requestReviewers openPr teamNames forcedReviewers {dry}
             when (not (null labelSlugs || dry)) $
               label labelSlugs
  where
    -- partition args into user logins, team slugs, and label slugs
    partitionedArgs : (List String, List String, List String)
    partitionedArgs = 
      let (userArgs, otherArgs) = partition (isPrefixOf "+") args
          (labelArgs, teams) = partition isHashPrefix otherArgs
          (users, labels) = mapHom (map $ drop 1) (userArgs, labelArgs)
      in  (users, teams, labels)

||| List teams for the configured org.
export
listOrgTeams : Config => Octokit =>
       Promise' ()
listOrgTeams @{config} =
  do teamNames <- sort <$> forceListTeamNames config.org
     renderIO . vsep $ annotate italic . pretty <$> teamNames

||| List members of a given team when the user executes
||| `harmony list <team>`.
export
list : Config => Octokit =>
       (team : String) 
    -> Promise' ()
list @{config} team =
  do teamMemberLogins <- sort <$> forceListTeamMembers config.org team
     teamMembersJson <- promiseAll =<< traverse forkedUser teamMemberLogins
     teamMembers <- traverse (either . parseUser) teamMembersJson
     renderIO . vsep $ putNameLn <$> teamMembers
  where
    forkedUser : (login : String) -> Promise' Future
    forkedUser = fork . ("user --json " ++)

    putNameLn : User -> Doc AnsiStyle
    putNameLn user =
      hsep [(fillBreak 15 . annotate italic $ pretty user.login), "-", (pretty user.name)]

data GraphArg : Type where
  TeamName : String -> GraphArg
  IncludeCompletedReviews : GraphArg

teamNameArg : GraphArg -> Maybe String
teamNameArg (TeamName n) = Just n
teamNameArg _ = Nothing

||| Parse arguments for the graph command.
export
parseGraphArgs : List String -> Either String (List GraphArg)
parseGraphArgs [] =
  Left "The graph command expects the name of a GitHub Team and optionally --completed as arguments."
parseGraphArgs (x :: y :: z :: xs) =
  Left "graph accepts at most one team name and the --completed flag."
parseGraphArgs args =
  case (traverse (parseCompletedFlag <||> parseTeamArg) args) of
       Just args => Right args
       Nothing   =>
         Left "The graph command expects the name of a GitHub Team and optionally --completed as arguments."
  where
    parseCompletedFlag : String -> Maybe GraphArg
    parseCompletedFlag "-c" = Just IncludeCompletedReviews
    parseCompletedFlag "--completed" = Just IncludeCompletedReviews
    parseCompletedFlag _ = Nothing

    parseTeamArg : String -> Maybe GraphArg
    parseTeamArg str = Just (TeamName str)

||| Graph the PR review workload for each member of the given team when
||| the user executes `harmony graph <team>`.
export
graph : Config => Octokit =>
        List GraphArg
     -> Promise' ()
graph @{config} args = do
  let includeCompletedReviews = find (\case IncludeCompletedReviews => True; _ => False) args
  let Just teamName = head' $ mapMaybe teamNameArg args
    | Nothing => reject "The graph command expects the name of a GitHub Team as an argument."
  teamMemberLogins <- forceListTeamMembers config.org teamName
  prs <- listPartitionedPRs 100 {pageBreaks=4}
  let (openReviewers, closedReviewers) = prs.allReviewers
  completedReviews <- 
    case (isJust includeCompletedReviews) of
         True  => Just <$> countReviewsByEachUser (combined prs)
         False => pure Nothing
  renderIO $ reviewsGraph closedReviewers openReviewers teamMemberLogins completedReviews

export
health : Config => Octokit =>
         Promise' ()
health @{config} = do
  prs <- listOpenPRs {pageBreaks = 4} 100
  renderIO $ healthGraph prs config.org config.repo

data IgnoreOpt = PRNum Nat

data ContributeArg = List | Checkout | Ignore IgnoreOpt | Skip Nat

skipArg : ContributeArg -> Maybe Nat
skipArg (Skip n) = Just n
skipArg _ = Nothing

ignorePRNums : List ContributeArg -> List Integer
ignorePRNums [] = []
ignorePRNums ((Ignore (PRNum k)) :: xs) = cast k :: ignorePRNums xs
ignorePRNums (_ :: xs) = ignorePRNums xs

contributeUsageError : String
contributeUsageError =
  "contribute's arguments must be -<num> to skip num PRs, --ignore (-i) <uri>/<pr-number>, --list to list PRs instead of picking the first one, or --checkout (-c) to checkout the branch needing review."

||| Parse arguments to the contribute subcommand.
export
parseContributeArgs : List String -> Either String (List ContributeArg)
parseContributeArgs [] = Right []
parseContributeArgs args =
  let (ignoreArgs, rest) = recombineIgnoreArgs args
      ignoreArgs' = Ignore <$> ignoreArgs
      rest' = (traverse (parseListFlag <||> parseSkipArg <||> parseCheckoutFlag) rest)
      in  maybeToEither contributeUsageError ((ignoreArgs' ++) <$> rest')
  where
    parseListFlag : String -> Maybe ContributeArg
    parseListFlag "-l" = Just List
    parseListFlag "--list" = Just List
    parseListFlag _ = Nothing

    parseCheckoutFlag : String -> Maybe ContributeArg
    parseCheckoutFlag "-c" = Just Checkout
    parseCheckoutFlag "--checkout" = Just Checkout
    parseCheckoutFlag _ = Nothing

    parseSkipArg : String -> Maybe ContributeArg
    parseSkipArg skipArg =
      case strM skipArg of
           (StrCons '-' skip) => map (Skip . cast) $ parsePositive skip
           _                  => Nothing

    -- expect a Nat or else a URI here of the form: https://github.com/<org>/<repo>/pull/<pr-number>
    parseIgnoreOpt : String -> Maybe IgnoreOpt
    parseIgnoreOpt str =
      let parts = split (== '/') str
          lastPart  = last parts
      in PRNum <$> parsePositive lastPart

    -- the --ignore option takes the next argument as its input so we will
    -- take two consecutive list elements and combine them for that option.
    -- The options will be returned separately and the rest will be left for
    -- later.
    recombineIgnoreArgs : List String -> (List IgnoreOpt, List String)
    recombineIgnoreArgs [] = ([], [])
    recombineIgnoreArgs ("-i" :: []) = ([], ["-i"])
    recombineIgnoreArgs ("--ignore" :: []) = ([], ["--ignore"])
    recombineIgnoreArgs ("-i" :: (x :: xs)) = 
      case parseIgnoreOpt x of
           Just opt => mapFst (opt ::) (recombineIgnoreArgs xs)
           Nothing  => mapSnd (\xs' => "-i" :: x :: xs') (recombineIgnoreArgs xs)
    recombineIgnoreArgs ("--ignore" :: (x :: xs)) =
      case parseIgnoreOpt x of
           Just opt => mapFst (opt ::) (recombineIgnoreArgs xs)
           Nothing  => mapSnd (\xs' => "--ignore" :: x :: xs') (recombineIgnoreArgs xs)
    recombineIgnoreArgs (x :: xs) = mapSnd (x ::) (recombineIgnoreArgs xs)

||| Present the user with a PR to review when they execute
||| `harmony contribute`.
export
contribute : Config => Octokit =>
             (args : List ContributeArg)
          -> Promise' ()
contribute @{config} args = do
  openPrs <- listPullRequests config.org config.repo (Just Open) 100
  let nonDraftPrs = filter (not . isDraft) openPrs
  myLogin <- login <$> getSelf
  let newIgnorePRNums = ignorePRNums args
  config' <-
    if (null newIgnorePRNums)
       then pure config
       else addIgnoredPRs config newIgnorePRNums
  -- options:
  let skip = fromMaybe 0 (head' $ mapMaybe skipArg args)
  let checkout = find (\case Checkout => True; _ => False) args
  let list = find (\case List => True; _ => False) args
  when (isJust checkout && isJust list) $
    reject "The --checkout and --list options are mutually exclusive"
  -- execution:
  let notMine = filter (not . isAuthor myLogin) nonDraftPrs
  let notIgnored = filter (isNotIgnored config') notMine
  let parted = partition (isRequestedReviewer myLogin) notIgnored
  let (requestedOfMe, others) = (mapHom $ sortBy (compare `on` .createdAt)) parted
  case list of
       Nothing  => pickOne config' skip checkout requestedOfMe others
       (Just _) => listSome skip requestedOfMe others

  where
    isNotIgnored : Config -> PullRequest -> Bool
    isNotIgnored config pr =
      isNothing $
        find (== pr.number) config.ignoredPRs

    pickOne : Config -> Nat -> Maybe ContributeArg -> List PullRequest -> List PullRequest -> Promise' ()
    pickOne config' skip checkout requestedOfMe others =
      let pr = head' . drop skip $ requestedOfMe ++ others
      in case pr of
           Nothing => reject "No open PRs to review!"
           Just pr => do
             whenJust (checkout $> pr.headRef) $ \branch => do
               checkoutBranch branch
             putStrLn  (pr.webURI @{config'})

    printDetails : PullRequest -> Doc AnsiStyle
    printDetails pr =
      let branch  = annotate italic (pretty pr.headRef)
          title   = pretty "  ├ title:  " <++> pretty pr.title
          created = pretty "  ├ created:" <++> pretty (show pr.createdAt)
          number  = pretty "  ├ number: " <++> annotate (color Green) (pretty pr.number)
          link    = pretty "  └ url:    " <++> annotate (color Blue) (pretty pr.webURI)
      in vsep [branch, title, created, number, link]

    goListSome : Bool -> List PullRequest -> Promise' ()
    goListSome direct prs = do
      let (pr :: rest) = prs
        | [] => pure ()
      let first = printDetails pr
      renderIO $
        indent (if direct then 0 else 2) $
          foldl (\acc, next => vsep [acc, emptyDoc, printDetails next]) first rest

    listSome : Nat -> List PullRequest -> List PullRequest -> Promise' ()
    listSome skip requestedOfMe others = do
      goListSome True (drop skip requestedOfMe)
      when (not $ null others) $ do
        putStrLn ""
        renderIO $ annotate bold "Your review not requested:"
        goListSome False (take 5 others)

||| Print the GitHub URI for the current branch when the user
||| executes `harmony branch`.
export
branch : Config => Promise' ()
branch @{config} = do
  branch <- currentBranch
  let org = config.org
  let repo = config.repo
  let uri = "https://github.com/\{org}/\{repo}/tree/\{branch}"
  putStrLn uri

data ProjectArg = Num Integer | Title String

Show ProjectArg where
  show (Num i) = "number \{show i}"
  show (Title str) = "\"\{str}\""

export
data QuickArg = ABugfix | AProject ProjectArg | IssueNumOrTitle String

export
parseQuickArgs : List String -> List QuickArg
parseQuickArgs [] = []
parseQuickArgs ("--bugfix" :: xs) = ABugfix :: parseQuickArgs xs
parseQuickArgs ("--project" :: numOrTitle :: xs) = 
  case parseInteger numOrTitle of
       Just n => AProject (Num n) :: parseQuickArgs xs
       Nothing => AProject (Title numOrTitle) :: parseQuickArgs xs
parseQuickArgs (titleStr :: xs) = IssueNumOrTitle titleStr :: parseQuickArgs xs

projectArgs : List QuickArg -> List ProjectArg
projectArgs = foldl go []
  where
    go : List ProjectArg -> QuickArg -> List ProjectArg
    go ps ABugfix = ps
    go ps (IssueNumOrTitle _) = ps
    go ps (AProject p) = p :: ps

titleArg : List QuickArg -> Maybe String
titleArg = foldl go Nothing
  where
    go : Maybe String -> QuickArg -> Maybe String
    go mstr ABugfix = mstr
    go mstr (AProject _) = mstr
    go Nothing (IssueNumOrTitle str) = Just str
    go (Just x) (IssueNumOrTitle str) = Just $ "\{x} \{str}"

titleOrNumberArg : List QuickArg -> IssueIdent
titleOrNumberArg args =
  go $ filter (\case ABugfix => False; _ => True) args

  where
    go : List QuickArg -> IssueIdent
    go [IssueNumOrTitle str] =
      if isHashPrefix str
         then IssueNumber $ drop 1 str
         else IssueTitle str
    go args = maybe NoInfo IssueTitle $ titleArg args

issueCategory : List QuickArg -> IssueCategory
issueCategory = maybe Feature (const Bugfix) . find (\case ABugfix => True; _ => False)

||| Quickly create a new GitHub issue and branch to go along with it.
export
quick : Config =>
        Octokit =>
        (args : List QuickArg)
     -> Promise' ()
quick @{config} args = do
  project <- maybeProject
  quickStartNewWork (issueCategory args)
                    (titleOrNumberArg args)
                    {project}

  where
    projectFromChoices : ProjectArg -> Maybe ProjectRef
    projectFromChoices (Num   i) = find ((i ==) . number) config.repoProjects
    projectFromChoices (Title t) = projectFromUnsluggifiedTitle config.repoProjects t

    maybeProject : Promise' (Maybe ProjectRef)
    maybeProject =
      case projectArgs args of
           (_ :: _ :: _) => reject "Only one project per new issue is currently supported"
           [arg] => maybe (reject "The specified project (\{show arg}) could not be found. Perhaps you need to run `harmony sync` to pick up a very new project?")
                          (pure . Just)
                          (projectFromChoices arg)
           []    => pure Nothing

