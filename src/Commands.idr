module Commands

import Data.Config
import Data.Date
import Data.List
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
import Graph
import Label
import PullRequest
import Reviewer
import User
import Util

import Language.JSON
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

export
sync : Config => Octokit =>
       Promise ()
sync = ignore $ syncConfig True

||| Provide information about who the current user is when
||| they execute `harmony whoami`.
export
whoami : Config => Git => Octokit =>
         Promise ()
whoami = printInfoOnSelf

||| Provide information on the curent user's recent work and currrent
||| workflow when they execute `harmony relfect`.
export
reflect : Config => Octokit =>
          Promise ()
reflect = reflectOnSelf

||| Print the URI for the current branch's PR or create a new PR if one
||| does not exist when the user executes `harmony pr`
export
pr : Config => Git => Octokit =>
     {default False isDraft : Bool}
  -> Promise ()
pr {isDraft} = do
  (Identified, pr) <- identifyOrCreatePR {isDraft} !currentBranch
    | _ => pure ()
  putStrLn pr.webURI

||| Apply the given labels to the current PR when the user executes
||| `harmony label <label> ...`.
export
label : Config => Git => Octokit =>
        (labels : List String)
     -> Promise ()
label @{config} labels =
  do (_, openPr) <- identifyOrCreatePR !currentBranch
     let finalLabels = unslugify config.repoLabels <$> labels
     allLabels <- addLabels openPr finalLabels
     renderIO $ vsep
       [ "Added" <++> putLabels finalLabels <+> " to PR."
       , pretty "All labels for PR of \{openPr.headRef}:" <++> putLabels allLabels <+> "."
       ]
  where
    ||| In order to support tab completion of multi-word labels, spaces have been turned into
    ||| another character to "slugify" the labels. Still, it is possible the user has entered
    ||| a label that literally contains the character used during slugification, so to
    ||| unslugify, we first see if a label appears in the configured list of labels. If it does
    ||| then we use it exactly but if it doesn't then we unslugify it before using it.
    unslugify : (configLabels : List String) -> (slugifiedLabel : String) -> String
    unslugify configLabels slugifiedLabel =
      case find (== slugifiedLabel) configLabels of
           Just label => label
           Nothing    => BashCompletion.unslugify slugifiedLabel

    putLabel : String -> Doc AnsiStyle
    putLabel = enclose "\"" "\"" . annotate (color Green) . pretty

    putLabels : List String -> Doc AnsiStyle
    putLabels = hcat . intersperse (pretty ", ") . map putLabel

||| Assign the given teams & users as reviewers when the user executes
||| `harmony assign ...`.
export
assign : Config => Git => Octokit => 
         (assignArgs : List String) 
      -> {default False dry : Bool} 
      -> Promise ()
assign args {dry} =
  do let (forcedReviewers, teamNames, labelSlugs) = partitionedArgs
     if (null forcedReviewers && null teamNames)
        then reject "The assign command expects one or more names of GitHub Teams or Users as arguments."
        else do (_, openPr) <- identifyOrCreatePR !currentBranch
                requestReviewers openPr teamNames forcedReviewers {dry}
                when (not (null labelSlugs || dry)) $
                  label labelSlugs
  where
    -- partition args into user logins, team slugs, and label slugs
    partitionedArgs : (List String, List String, List String)
    partitionedArgs = 
      let (userArgs, otherArgs) = partition (isPrefixOf "+") args
          (labelArgs, teams) = partition (isPrefixOf "#") otherArgs
          (users, labels) = mapHom (map $ drop 1) (userArgs, labelArgs)
      in  (users, teams, labels)

||| List members of a given team when the user executes
||| `harmony list <team>`.
export
list : Config => Octokit =>
       (team : String) 
    -> Promise ()
list @{config} team =
  do teamMemberLogins <- sort <$> listTeamMembers config.org team
     teamMembersJson <- promiseAll =<< traverse forkedUser teamMemberLogins
     teamMembers <- traverse (either . parseUser) teamMembersJson
     renderIO . vsep $ putNameLn <$> teamMembers
  where
    forkedUser : (login : String) -> Promise Future
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
     -> Promise ()
graph @{config} args = do
  let includeCompletedReviews = find (\case IncludeCompletedReviews => True; _ => False) args
  let Just teamName = head' $ mapMaybe teamNameArg args
    | Nothing => reject "The graph command expects the name of a GitHub Team as an argument."
  teamMemberLogins <- listTeamMembers config.org teamName
  prs <- listPartitionedPRs 100 {pageBreaks=4}
  let (openReviewers, closedReviewers) = prs.allReviewers
  completedReviews <- 
    case (isJust includeCompletedReviews) of
         True  => countReviewsByEachUser (combined prs)
         False => pure empty
  renderIO $ reviewsGraph closedReviewers openReviewers teamMemberLogins (Just completedReviews)

export
health : Config => Octokit =>
         Promise ()
health = do
  prs <- listOpenPRs {pageBreaks = 4} 100
  renderIO $ healthGraph prs

(<||>) : Alternative t => (a -> t b) -> (a -> t b) -> a -> t b
(<||>) f g x = f x <|> g x

infixr 2 <||>

||| Parse arguments for the graph command.
export
parseGraphArgs : List String -> Either String (List GraphArg)
parseGraphArgs [] = Right []
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

data ContributeArg = Checkout | Skip Nat

skipArg : ContributeArg -> Maybe Nat
skipArg (Skip n) = Just n
skipArg _ = Nothing

||| Present the user with a PR to review when they execute
||| `harmony contribute`.
export
contribute : Config => Git => Octokit =>
             (args : List ContributeArg)
          -> Promise ()
contribute @{config} args =
  do openPrs <- listPullRequests config.org config.repo (Just Open) 100
     myLogin <- login <$> getSelf
     let skip = fromMaybe 0 (head' $ mapMaybe skipArg args)
     let checkout = find (\case Checkout => True; _ => False) args
     let filtered = filter (not . isAuthor myLogin) openPrs
     let parted = partition (isRequestedReviewer myLogin) filtered
     let (mine, theirs) = (mapHom $ sortBy (compare `on` .createdAt)) parted
     let pr = head' . drop skip $ mine ++ theirs
     case pr of
          Nothing => reject "No open PRs to review!"
          Just pr => do
            whenJust (checkout $> pr.headRef) $ \branch => do
              checkoutBranch branch
            putStrLn  pr.webURI

||| Parse arguments to the contribute subcommand.
export
parseContributeArgs : List String -> Either String (List ContributeArg)
parseContributeArgs [] = Right []
parseContributeArgs (_ :: _ :: _ :: _) =
  Left "contribute's arguments must be either -<num> to skip num PRs or --checkout (-c) to checkout the branch needing review."
parseContributeArgs args =
  case (traverse (parseSkipArg <||> parseCheckoutFlag) args) of
       Just args => Right args
       Nothing   =>
         Left "contribute's arguments must be either -<num> to skip num PRs or --checkout (-c) to checkout the branch needing review."
  where
    parseCheckoutFlag : String -> Maybe ContributeArg
    parseCheckoutFlag "-c" = Just Checkout
    parseCheckoutFlag "--checkout" = Just Checkout
    parseCheckoutFlag _ = Nothing

    parseSkipArg : String -> Maybe ContributeArg
    parseSkipArg skipArg =
      case unpack skipArg of
           ('-' :: skip) => map (Skip . cast) . parsePositive $ pack skip
           _             => Nothing

||| Print the GitHub URI for the current branch when the user
||| executes `harmony branch`.
export
branch : Config => Git => Promise ()
branch @{config} = do
  branch <- currentBranch
  let org = config.org
  let repo = config.repo
  let uri = "https://github.com/\{org}/\{repo}/tree/\{branch}"
  putStrLn uri
