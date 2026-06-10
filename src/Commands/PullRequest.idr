module Commands.PullRequest

import Commands.Reviewer
import Commands.Quick

import Data.Config
import Data.Either
import Data.Fin.Extra
import Data.Fuel
import Data.Issue
import Data.List
import Data.List1
import Data.Nat
import Data.Pagination
import Data.Promise
import Data.PullRequest
import Data.Review
import Data.SortedMap
import Data.String
import Data.String.Extra
import Data.User

import FFI.Concurrency
import FFI.GitHub
import JSON.Parser
import Language.JSON.Accessors
import System
import System.File
import System.Git
import Theme
import Util
import Util.Github
import Util.Jira

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

public export
data IdentifiedOrCreated = Identified | Created

public export
data CreatePRResult = Actual IdentifiedOrCreated PullRequest
                    | Hypothetical String

public export
record PRHistory where
  constructor MkPRHistory
  openPRs   : List PullRequest
  closedPRs : List PullRequest

empty : PRHistory
empty = MkPRHistory { openPRs = [], closedPRs = [] }

||| Produce a tuple of (open, closed) from the PR History.
export
tuple : PRHistory -> (List PullRequest, List PullRequest)
tuple (MkPRHistory openPRs closedPRs) = (openPRs, closedPRs)

export
combined : PRHistory -> List PullRequest
combined history = history.openPRs ++ history.closedPRs

||| Extract a tuple of open and closed PR reviewer names
||| from a PR history. A given reviewer's login appears
||| in the result once for each review request.
||| The tuple is structured (open list, closed list)
export
(.allReviewers) : PRHistory -> (List String, List String)
(.allReviewers) = mapHom (join . map reviewers) . tuple

||| Extract a list of author logins. A given author's login
||| appears in the result once for each PR authored.
export
(.allAuthors) : PRHistory -> List String
history.allAuthors = author <$> (history.openPRs ++ history.closedPRs)

partition : List PullRequest -> PRHistory
partition = uncurry MkPRHistory . partition ((== Open) . (.state))

||| Create a fork of this program that retrieves the given page of PRs
||| and outputs the result as JSON.
forkedPRs : (filter : Maybe GitHubPRState) -> (perPage : Nat) -> (currentPageIdx : Nat) -> (currentPageSize : Nat) -> (x : ()) -> Promise' Future
forkedPRs filter perPage page _ _ = fork "pulls --json \{filterString} \{show perPage} \{show page}"
  where
    filterString : String
    filterString = case filter of
                        Nothing     => "none"
                        Just Open   => "open"
                        Just Closed => "closed"

||| Grab all of the given pages of PRs and create a list of PRs from them.
list' : (filter : Maybe GitHubPRState) -> Pagination _ _ _ () -> Promise' (List PullRequest)
list' filter pgs = do
  prJsons <- promiseAll =<< traverse' (forkedPRs filter)  pgs
  pulls   <- either $ traverse (array parsePR) prJsons
  pure $ join pulls

||| Get the most recent open PRs by creation date.
|||
||| @prCount The number of PRs to retrieve.
||| @pageBreaks The number of pages over which to break the requests up.
export
listOpenPRs : Config => Octokit =>
              {default 0 pageBreaks : Nat}
           -> (prCount : Fin 101)
           -> Promise' (List PullRequest)
listOpenPRs @{config} {pageBreaks} prCount with ((finToNat prCount) `isGT` 0, pageBreaks `isGT` 0)
  _ | (No _, _) = pure empty
  _ | (_, No _) = listPullRequests config.org config.repo (Just Open) prCount
  _ | (Yes prf, Yes prf') with ((S pageBreaks) `isLTE` (finToNat prCount))
    _ | (No _)      = list' (Just Open) (metaPages  (finToNat prCount)  1)
    _ | (Yes prf'') = list' (Just Open) (metaPages' (finToNat prCount) (S pageBreaks))

||| Grab all of the given pages of PRs and create a history from them.
partition' : Pagination _ _ _ () -> Promise' PRHistory
partition' = map partition . (list' Nothing)

||| Get the most recent PRs by creation date and partition them
||| into open and closed PRs.
|||
||| @prCount The number of PRs to retrieve.
||| @pageBreaks The number of pages over which to break the requests up.
export
listPartitionedPRs : Config => Octokit =>
                     {default 0 pageBreaks : Nat}
                  -> (prCount : Fin 101)
                  -> Promise' PRHistory
listPartitionedPRs @{config} {pageBreaks} prCount with ((finToNat prCount) `isGT` 0, pageBreaks `isGT` 0)
  _ | (No _, _) = pure empty
  _ | (_, No _) = partition <$> listPullRequests config.org config.repo Nothing prCount
  _ | (Yes prf, Yes prf') with ((S pageBreaks) `isLTE` (finToNat prCount))
    _ | (No _)      = partition' (metaPages  (finToNat prCount)  1)
    _ | (Yes prf'') = partition' (metaPages' (finToNat prCount) (S pageBreaks))

||| List the names of all reviewers of open and closed PRs. A
||| given reviewer's login appears in the result once for each
||| review request.
||| The tuple is structured (open list, closed list)
export
listReviewers : Config => Octokit =>
                {default 0 pageBreaks : Nat}
             -> (prCount : Fin 101)
             -> Promise' (List String, List String)
listReviewers = map (.allReviewers) . (listPartitionedPRs {pageBreaks})

||| Get all of the reviews on the given PRs.
reviewsForPrs : Config => Octokit =>
                List PullRequest
             -> Promise' (List Review)
reviewsForPrs prs = do
    reviewsJson <- promiseAll =<< traverse forkedReviews prs
    -- ^ list of JSON Arrays
    reviews <- either $ traverse (array parseReview) reviewsJson
    pure $ join reviews
  where
    forkedReviews : PullRequest -> Promise' Future
    forkedReviews = fork . ("reviews --json " ++) . show . number

||| Get the reviews on the given PRs by the given user.
export
reviewsByUser : Config => Octokit =>
                (author : String)
             -> List PullRequest
             -> Promise' (List Review)
reviewsByUser author prs = do
  let filteredPrs = filter (\pr => not $ isAuthor author pr || isRequestedReviewer author pr) prs
  -- ^ we know we aren't looking for reviews on the author's PRs.
  reviews <- reviewsForPrs filteredPrs
  pure $ filter (isAuthor author) reviews

||| Get reviews for the given PRs broken down by review author.
export
reviewsByEachUser : Config => Octokit =>
                    List PullRequest
                 -> Promise' (SortedMap String (List Review))
reviewsByEachUser prs = do
  reviews <- reviewsForPrs prs
  let groupedReviews = groupAllWith (.author) reviews
  let taggedGroups = groupedReviews <&> (\rs => ((head rs).author, forget rs))
  pure $ fromList taggedGroups

||| Get a map from each user to the number of reviews that user gave over the
||| given pull requests. Any user that did not give reviews will not be included
||| in the resulting map.
|||
||| IMPORTANT: This makes one API request per PR so it can be expensive and slow.
export
countReviewsByEachUser : Config => Octokit =>
                         List PullRequest
                      -> Promise' (SortedMap String Nat)
countReviewsByEachUser = pure . (map length) <=< reviewsByEachUser

||| Request reviews.
||| @ pullRequest     The Pull Request for which reviews should be requested.
||| @ teamNames       The slugs of teams from which to draw potential review candidates.
||| @ forcedReviewers The logins of users to force review from (in addition to the reviewer
|||                   chosen from the selected teams).
export
requestReviewers : Config => Octokit => 
                   PullRequest 
                -> (teamNames : List String) 
                -> (forcedReviewers : List String) 
                -> {default False dry: Bool} 
                -> Promise' ()
requestReviewers @{config} pr teamNames forcedReviewers {dry} = do 
  (openReviewers, closedReviewers) <- listReviewers 100 {pageBreaks=4}
  teamMembers <- join <$> traverse (forceListTeamMembers config.org) teamNames

  chosenUser <- if config.requestUsers
                     then let chosenCandidates = chooseReviewers closedReviewers openReviewers teamMembers [] pr.author
                          in  randomReviewer chosenCandidates
                     else pure Nothing

  let users = (toList chosenUser) ++ forcedReviewers
  let teams = if config.requestTeams then teamNames else []
  when (not dry) $ do
    ignore $ addPullReviewers config.org config.repo pr.number users teams
    whenJust chosenUser $ \cu =>
      case config.commentOnRequest of
           None => pure ()
           AtMention => createComment config.org config.repo pr.number (mentionPrComment cu)
           Name      => do user <- getUser cu
                           createComment config.org config.repo pr.number (namePrComment user)
  if null users && config.requestUsers
    then putStrLn . renderString $ vsep [
                    annotate (color Yellow) $ pretty "Could not pick a user from the given Team "
                  , pretty "(perhaps the only option was the author of the pull request?)."
                  , pretty "Requested review from \{teamNotice teams} for the open PR "
                  , pretty "for the current branch (\{pr.webURI})."
                  ]
    else putStrLn . renderString $ vsep [
                    pretty "Requested review from \{userNotice chosenUser}\{teamNotice teams} for the open PR "
                  , pretty "for the current branch (\{pr.webURI})."
                  ]
  where
    csv : List String -> String
    csv = renderString . encloseSep emptyDoc emptyDoc (pretty ", ") . map (annotate (color Green) . pretty)

    userNotice : (chosenReviewer : Maybe String) -> String
    userNotice Nothing       = case forcedReviewers of
                                    []         => "no users"
                                    reviewers  => "\{csv reviewers}"
    userNotice (Just chosen) = "\{csv (chosen :: forcedReviewers)}"

    teamNotice : List String -> String
    teamNotice []     = ""
    teamNotice [name] = " and team \{csv [name]}"
    teamNotice names  = " and teams \{csv names}"

    prComment : String -> String
    prComment userString = """
    :musical_note: Harmoniously requested review from \{userString}.
    """

    mentionPrComment : String -> String
    mentionPrComment chosenUser = prComment "@\{chosenUser}"

    namePrComment : User -> String
    namePrComment chosenUser = prComment "\{chosenUser.name}"

export
convertPRToDraft : Config => Octokit =>
                   PullRequest
                -> Promise' PullRequest
convertPRToDraft @{config} pr = do
  prId <- getPullRequestGraphQlId config.org config.repo pr.number
  markPullRequestDraft prId

export
convertPRToReady : Config => Octokit =>
                   PullRequest
                -> Promise' PullRequest
convertPRToReady @{config} pr = do
  prId <- getPullRequestGraphQlId config.org config.repo pr.number
  markPullRequestReady prId

record BranchInferredData where
  constructor MkInferredData

  ||| The title prefix is to be prepended to the new PR title.
  titlePrefix : Maybe String
  ||| The body prefix is to be prepended to the new PR body.
  buildBodyPrefix  : Maybe ((branchName : String) -> Promise' String)
  ||| The default title is an option the user may choose to avoid needing to
  ||| specify a title now when one can be borrowed from a GitHub issue.
  defaultTitle : Maybe String
  ||| A best guess of the base branch for the new PR based on information that
  ||| might be stored in an issue.
  baseBranchGuess : Maybe String

noInferredData : BranchInferredData
noInferredData = MkInferredData Nothing Nothing Nothing Nothing

relatedToPrefix : (issueNumber : String) -> String
relatedToPrefix issueNumber = "Related to #\{issueNumber}"

titlePrefixForBranch : Config => (branch : String) -> String
titlePrefixForBranch @{config} branch =
  if isBugfixBranch branch
     then maybe "" (++ " ") config.bugfixPRTitlePrefix
     else ""

removeCommentOpenTag : String -> String
removeCommentOpenTag str =
  if "<!--" `isPrefixOf` str
     then drop 4 str
     else str

removeCommentTags : String -> String
removeCommentTags = unlines . map removeCommentOpenTag . filter (/= "-->") . lines

issueDescriptionPrefix : (maybeTree : String) -> Issue -> String
issueDescriptionPrefix maybeTree issue =
  """
  <!--
  \{maybeTree}
  ## GitHub Issue
  \{issue.title}
  --
  \{removeCommentTags issue.body}
  -->
  """

issueBodyPrefix : (maybeTree : String) -> Issue -> String
issueBodyPrefix maybeTree issue =
  """
  \{issueDescriptionPrefix maybeTree issue}

  \{relatedToPrefix (show issue.number)}
  """

||| Get the chain of PRs that lead from the given branch to the configured
||| mainBranch or any other terminal branch along the way. The list will be in
||| merge-order. The list does not contain the mainBranch (or any other branch
||| along the way that has no PRs open against it) but such a terminal branch
||| is the second element of the returned tuple.
|||
||| @return (list of PRs, terminal branch)
export
upstreamPrChain : Config => Octokit => Fuel -> (branch : String) -> Promise' (List PullRequest, String)
upstreamPrChain Dry branch = pure ([], branch)
upstreamPrChain @{config} (More fuel) branch =
  if branch == config.mainBranch
     then pure ([], branch)
     else do (prForBranch :: _) <- listPRsForBranch config.org config.repo branch
               | [] => pure ([], branch)
             mapFst (prForBranch ::) <$> upstreamPrChain fuel prForBranch.baseRef

||| Get the chain of PRs that lead from the given branch to some downstream PR
||| with no PRs based off of it. The branch given is not included.
|||
||| There can be more than one downstream PR for any given PR but this function
||| currently just takes the first one and continues on ignoring all others.
||| That is a limitation, not a desired behavior.
|||
||| @return list of PRs
export
downstreamPrChain : Config => Octokit => Fuel -> (branch : String) -> Promise' (List PullRequest)
downstreamPrChain Dry _ = pure []
downstreamPrChain @{config} (More fuel) branch = do
  (prForBranch :: _) <- listPRsForBaseBranch config.org config.repo branch
    | [] => pure []
  (prForBranch ::) <$> downstreamPrChain fuel prForBranch.headRef

public export
data RenderFormat = Markdown | Shell

export
data PrTreeNode : Type where
  Branch : (symbol : String) -> (name : String) -> (title : Maybe String) -> PrTreeNode
  PR : {default False marked : Bool} -> (symbol : String) -> PullRequest -> PrTreeNode

||| Generate a PR tree.
|||
||| If you pass `branch` (and optionally a presumptive PR title for that
||| branch), that is the leaf of the tree. All PRs between that and the
||| terminal branch should be passed as the next argument. Then the terminal
||| branch (e.g. the `mainBranch` of the repo, usually) gets passed in last.
|||
||| The "current" PR is part of the `prs` list with any PRs based off of the
||| current PR being in the `downstreamPrs` list. You can always pass an empty
||| `downstreamPrs` to print only from the current PR down to the main branch.
export
prTree : (branch : Maybe String)
      -> (title : Maybe String)
      -> (downstreamPrs : List PullRequest)
      -> List PullRequest
      -> (terminalBranch : String)
      -> List PrTreeNode
prTree branch title downstreamPrs prs terminalBranch = 
  (Branch terminus terminalBranch Nothing) :: go True branch (reverse prs) ++ go False Nothing downstreamPrs

  where
    terminus : String
    terminus = "⨀"

    arrow : String
    arrow = "↖"

    go : (markLast : Bool) -> (branch : Maybe String) -> List PullRequest -> List PrTreeNode
    go _ (Just branch) [] = [Branch arrow branch title]
    go _ Nothing       [] = []
    go mark branch (pr :: prs) =
      -- we special case the last PR in the list to mark it as "current" unless there is a branch specified.
      if mark && null prs && isNothing branch
         then [PR {marked = True} arrow pr]
         else PR arrow pr :: go mark branch prs

||| Render a PR tree.
export
renderPrTree : Config => RenderFormat -> List PrTreeNode -> String
renderPrTree @{config} format = 
  snd . foldl renderNode (0, "")

  where
    mdIndent : Nat -> String -> String
    mdIndent i = (replicate (S i) '>' ++ " " ++)

    shellIndent : Nat -> Doc ann -> Doc ann
    shellIndent i = indent (cast $ 1 + i * 4)

    marker : (marked : Bool) -> Doc AnsiStyle
    marker False = ""
    marker True = theme' Current "▪ "

    markerLines : (marked : Bool) -> List String
    markerLines False = []
    markerLines True = ["**[[** -> _you are here_ <- **]]**"]

    renderNode : (Nat, String) -> PrTreeNode -> (Nat, String)
    renderNode (idx, acc) (Branch symbol name title) =
      let title' = maybe "" (\t => "\n" ++ mdIndent idx "**\{t}**") title
          next = \str => (S idx, acc ++ str ++ "\n")
      in next $ 
        case format of
             Markdown => mdIndent idx "\{symbol} `\{name}`\{title'}"
             Shell    => renderString $ 
                           shellIndent idx $ 
                             (pretty symbol) <++> (theme' Special $ pretty $ fromMaybe name title)
    renderNode (idx, acc) (PR {marked} symbol pr) =
      let next = \str => (S idx, acc ++ str ++ "\n")
          uri = webURI' config.org config.repo pr
      in next $
        case format of
             Markdown => 
               joinBy "\n" $
                 mdIndent idx <$> (("\{symbol} `\{pr.headRef}` (\{uri})") ::
                                   (markerLines marked)
                                   `snoc` "**\{pr.title}**"
                                  )
             Shell    => 
               renderString $
                 shellIndent idx $
                   vsep [ (pretty symbol) <++> (marker marked) <+> (theme' Data (pretty pr.title))
                        , indent 2 $ "├" <++> (pretty pr.headRef)
                        , indent 2 $ "└" <++> annotate italic (pretty uri)
                        ]

buildIssueBodyPrefix : Config => Octokit => (branch : String) -> Issue -> (baseBranch : String) -> Promise' String
buildIssueBodyPrefix @{config} branch issue baseBranch = do
  tree <- maybePrTree
  pure $ issueBodyPrefix tree issue
  where
    maybePrTree : Promise' String
    maybePrTree =
      if config.addPrTreeDescription && (baseBranch /= config.mainBranch)
         then do (prs, terminalBranch) <- upstreamPrChain (limit 10) baseBranch
                 let nodes = prTree (Just branch) (Just issue.title) [] prs terminalBranch
                 let tree = renderPrTree Markdown nodes
                 pure """
                      ## PR Tree
                      \{tree}
                      """
         else pure ""

githubInferredBranchInfo : Config => Octokit => (branch : String) -> Promise' BranchInferredData
githubInferredBranchInfo @{config} branch =
  case issueNumber of
    Nothing => pure noInferredData
    Just issueNum => do
      issue <- getIssue config.org config.repo issueNum
      pure $ 
        MkInferredData Nothing 
                       (Just $ buildIssueBodyPrefix branch issue)
                       (Just $ titlePrefixForBranch branch ++ issue.title)
                       issue.baseBranchGuess

  where
    issueNumber : Maybe String
    issueNumber = parseGithubIssueNumber branch

||| Get any inferred title prefix, body prefix, or default title from the
||| current branch.
|||
||| The idea in any case is to pass along the information parsed from the
||| branch name to GitHub either via the PR's title or part of its body so that
||| an issue referenced by the branch is tracked in relation to the new PR.
|||
||| For GitHub only, the issue title is suggested as the default PR title.
|||
||| If a GitHub issue is found, the body of the GitHub issue is also added
||| (commented out) to the body prefix as additional context.
|||
||| When the `addPrTreeDescription` Config option is set and a GitHub issue is
||| found, a tree of branches will be added to the body prefix if the `--into`
||| branch is not the configured `mainBranch`.
getInferredBranchInfo : Config => Octokit => (branch : String) -> Promise' BranchInferredData
getInferredBranchInfo @{config} branch =
    case config.branchParsing of
         Jira   => pure (MkInferredData ((++ " - ") <$> parseJiraSlug branch) Nothing Nothing Nothing)
         --               ^ Jira slugs become a title prefix
         Github => githubInferredBranchInfo branch
         None   => pure noInferredData

||| A GitHub URL at which a PR can be created for the given branch.
prCreationUrl : (org : String) -> (repo : String) -> (branch : String) -> (intoBranch : Maybe String) -> String
prCreationUrl org repo branch intoBranch =
  "https://github.com/\{org}/\{repo}/compare/\{into}\{branch}?expand=1"
    -- NOTE: I would love to be able to create the above 
    --       URL such that the page opens with "draft" 
    --       as the default if the PR was requested to 
    --       be a draft via the CLI but I have not found 
    --       a way to do that yet.
    where
      into : String
      into = case intoBranch of
                  Just branch => "\{branch}..."
                  Nothing     => ""

||| If a PR can be found on GitHub for the current branch, that PR is returned.
|||
||| If no PR can be found on GitHub, the result depends on whether the current
||| `stdout` is a TTY terminal or not. Given a TTY terminal, the user is walked
||| through creating a new PR in-terminal. Otherwise, a GitHub URL is returned
||| that, when visited, will allow the user to create a PR for the current
||| branch in-browser.
export
identifyOrCreatePR : Config => Octokit => 
                     {default False markAsDraft : Bool}
                  -> {default False createIssueForPR : Bool}
                  -> {default Nothing intoBranch : Maybe String}
                  -> (branch : String) 
                  -> Promise' CreatePRResult
identifyOrCreatePR @{config} {markAsDraft} {createIssueForPR} {intoBranch} branch = do
  [openPr] <- listPRsForBranch config.org config.repo branch
    | [] => do
        when (createIssueForPR && isJust (parseGithubIssueNumber branch)) $
          reject "The current branch already appears to reference a GitHub issue; --issue would create a duplicate issue."
        case !(isTTY stdout) of
             True  => Actual Created <$> createPR
             False => if createIssueForPR
                         then reject "The --issue option requires an interactive terminal because Harmony needs to prompt for issue details."
                         else pure (Hypothetical $ prCreationUrl config.org config.repo branch intoBranch)
    | _  => reject "Multiple PRs for the current brach. Harmony only handles 1 PR per branch currently."
  when createIssueForPR $
    reject "The --issue option is only supported when creating a new PR."
  pure (Actual Identified openPr)
    where
      continueGivenUncommittedChanges : Promise' Bool
      continueGivenUncommittedChanges = do
        case !uncommittedChanges of
             Just files => do
               putStrLn "The following files have uncommitted changes:"
               putStrLn files
               yesNoPrompt "Would you like to continue creating a Pull Request anyway?"
             Nothing => pure True

      continueGivenStagedChanges : Promise' Bool
      continueGivenStagedChanges = do
        case !stagedChanges of
             Just files => do
               putStrLn "The following files have staged but uncommitted changes:"
               putStrLn files
               yesNoPrompt "Would you like to continue creating a Pull Request anyway?"
             Nothing => pure True

      promptForBaseBranch : Promise' String
      promptForBaseBranch = do
         putStrLn "What branch are you merging into (ENTER for default: \{config.mainBranch})?"
         orIfEmpty (Just config.mainBranch) . trim <$> getLine

      getBaseBranch : (intoBranch : Maybe String) -> (baseBranchGuess : Maybe String) -> Promise' String
      getBaseBranch (Just branch) _ = do
         putStrLn "Will merge into \{branch}."
         pure branch
      getBaseBranch Nothing (Just branchGuess) = do
        True <- yesNoPrompt "Do you want to merge into \{branchGuess}?"
          | False => promptForBaseBranch
        pure branchGuess
      getBaseBranch Nothing Nothing =
        promptForBaseBranch

      inlineDescriptionPrompt : String
      inlineDescriptionPrompt =
        "What would you like the description to be (two blank lines to finish)?"

      notEmptyString : String -> Maybe String
      notEmptyString "" = Nothing
      notEmptyString str = Just str

      prTitlePrompt : Maybe String -> Promise' String
      prTitlePrompt defaultTitle =
        let fallbackTitle = "New PR"
        in  offerRetry 
              "PR title cannot be an empty string."
              "Did not find a non-empty value for a PR title. Will use '\{fallbackTitle}'"
              fallbackTitle
              (notEmptyString <$> getLineEnterForDefault "What would you like the PR title to be?"
                                                         defaultTitle)

      createPR : Promise' PullRequest
      createPR = do
        -- create a remote tracking branch if needed
        whenNothing !remoteTrackingBranch $ do
          putStrLn "Creating a new remote branch..."
          pushNewBranch config.defaultRemote branch

        -- ask if we should continue despite uncommitted changes
        True <- continueGivenUncommittedChanges
          | False => reject "Not creating a PR (for now)..."
        True <- continueGivenStagedChanges
          | False => reject "Not creating a PR (for now)..."

        -- ask if unpushed commits should be pushed
        whenJust !unpushedCommits $ \unpushedString => do
          putStrLn "The following commits have not been pushed:\n"
          putStrLn unpushedString
          putStrLn "\n"
          pushUnpushedChanges <-
            yesNoPrompt "Would you like to push these changes before creating a PR?"
          when pushUnpushedChanges push

        -- proceed to creating a PR
        putStrLn "Creating a new PR for the current branch (\{branch})."
        inferredBranchInfo <- getInferredBranchInfo branch

        baseBranch <- getBaseBranch intoBranch inferredBranchInfo.baseBranchGuess

        let issueForPrPromise : Promise' (Maybe Issue)
            issueForPrPromise =
              if createIssueForPR
                 then Just <$> createNewIssueWithMessage "Creating a new GitHub issue for this PR." baseBranch Nothing
                 else pure Nothing
        issueForPr <- issueForPrPromise

        let titlePrefix = fromMaybe "" inferredBranchInfo.titlePrefix
        bodyPrefix <- case issueForPr of
                           Just issue => buildIssueBodyPrefix branch issue baseBranch
                           Nothing => maybe (pure "") ($ baseBranch) inferredBranchInfo.buildBodyPrefix

        let defaultTitle = case issueForPr of
                                Just issue => Just issue.title
                                Nothing => inferredBranchInfo.defaultTitle

        title <- (titlePrefix ++) <$> (prTitlePrompt defaultTitle)

        -- either get the description at the command line or open an editor
        -- with a template if available
        templateFilePath <- relativeToRoot ".github/PULL_REQUEST_TEMPLATE.md"
        description <- case config.editor of
                            Nothing => inlineDescription inlineDescriptionPrompt bodyPrefix
                            Just ed => either (const "") id <$>
                                         editorDescription ed (Just templateFilePath) bodyPrefix

        putStrLn "Creating PR..."
        putStrLn branch

        GitHub.createPR {markAsDraft} config.org config.repo branch baseBranch title description
