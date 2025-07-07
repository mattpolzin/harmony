module Commands

import Commands.Graph
import Commands.Label
import Commands.PullRequest
import Commands.Reviewer
import Commands.User

import Data.Config
import Data.Date
import Data.DPair
import Data.Either
import Data.List
import Data.List1
import Data.Promise
import Data.PullRequest
import Data.SortedMap
import Data.String
import Data.User

import BashCompletion
import Config
import FFI.Concurrency
import FFI.Git
import FFI.GitHub
import Util

import JSON.Parser
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

export
sync : Config => Octokit =>
       Promise' ()
sync = ignore $ syncConfig True

||| Provide information about who the current user is when
||| they execute `harmony whoami`.
export
whoami : Config => Git => Octokit =>
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
label : Config => Git => Octokit =>
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
  "pr's arguments must be #<label>, --into <branch-name>, or --draft to create a draft PR."

data IntoOpt = Branch (Exists String.NonEmpty)

data PrArg = Draft | Into IntoOpt | Label String

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
      rest' = (traverse (parseDraftFlag <||> parseLabelArg) rest)
      in  maybeToEither prUsageError ((intoArgs' ++) <$> rest')
  where
    parseDraftFlag : String -> Maybe PrArg
    parseDraftFlag "--draft" = Just Draft
    parseDraftFlag _ = Nothing

    parseLabelArg : String -> Maybe PrArg
    parseLabelArg labelArg =
      case unpack labelArg of
           ('#' :: label) => Just . Label $ pack label
           _              => Nothing

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

||| Print the URI for the current branch's PR or create a new PR if one
||| does not exist when the user executes `harmony pr`. Supports creation
||| of draft PRs (default False) and can accept any number of labels to apply
||| to the new or current PR.
export
pr : Config => Git => Octokit =>
     (args : List PrArg)
  -> Promise' ()
pr args =
  let intoBranch = intoArg
  in
  if all isHashPrefix labelSlugs
     then do Actual actionTaken pr <- identifyOrCreatePR {isDraft} {intoBranch} !currentBranch
               | Hypothetical url => putStrLn url
             case actionTaken of
                  Identified => putStrLn pr.webURI
                  Created    => pure ()
             when (not $ null labelSlugs) $
               label labelSlugs
             whenJust intoBranch $ \branch =>
               if not (branch `isSuffixOf` pr.baseRef)
                 then reject "Setting the --into branch (base ref) for an existing PR is not supported (yet). Base ref will remain \{pr.baseRef}"
                 else pure ()
             when (isDraft && not pr.isDraft) $ do
               putStrLn ""
               True <- yesNoPrompt {defaultAnswer = False} "Are you sure you want to convert the existing PR for the current branch to a draft?"
                 | False => putStrLn "No worries, the PR won't be converted to a draft."
               ignore $ convertPRToDraft pr
               putStrLn "The PR for the current branch has been converted to a draft."
     else reject "The pr command only accepts labels prefixed with '#', the --into option, and the --draft flag."

  where
    isDraft : Bool
    isDraft = isJust $ find (\case Draft => True; _ => False) args

    labelSlugs : List String
    labelSlugs = foldr (\case (Label l) => (l ::); _ => id) [] args

    intoArg : Maybe String
    intoArg =
      foldMap (\case (Into (Branch name)) => Just (value name.snd); _ => Nothing) args

||| Request review from the given teams & users as reviewers when the user executes
||| `harmony request ...`.
export
request : Config => Git => Octokit => 
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
  do teamNames <- sort <$> forceListTeams config.org
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

namespace TestParseGraphArgs

  testJustTeamName : Commands.parseGraphArgs ["team1"] === Right [TeamName "team1"]
  testJustTeamName = Refl

  testRequiresOneArgument : Commands.parseGraphArgs [] === Left "The graph command expects the name of a GitHub Team and optionally --completed as arguments."
  testRequiresOneArgument = Refl

  testAcceptsAtMostTwoArguments : Commands.parseGraphArgs ["a", "b", "c"] === Left "graph accepts at most one team name and the --completed flag."
  testAcceptsAtMostTwoArguments = Refl

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
      case unpack skipArg of
           ('-' :: skip) => map (Skip . cast) . parsePositive $ pack skip
           _             => Nothing

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
contribute : Config => Git => Octokit =>
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
branch : Config => Git => Promise' ()
branch @{config} = do
  branch <- currentBranch
  let org = config.org
  let repo = config.repo
  let uri = "https://github.com/\{org}/\{repo}/tree/\{branch}"
  putStrLn uri

