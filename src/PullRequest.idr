module PullRequest

import Data.Config
import Data.Either
import Data.Fin.Extra
import Data.Fuel
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
import FFI.Concurrency
import FFI.Git
import FFI.GitHub
import JSON.Parser
import Language.JSON.Accessors
import Reviewer
import System
import System.File
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

public export
data IdentifiedOrCreated = Identified | Created

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
forkedPRs : (filter : Maybe GitHubPRState) -> (perPage : Nat) -> (currentPageIdx : Nat) -> (currentPageSize : Nat) -> (x : ()) -> Promise Future
forkedPRs filter perPage page _ _ = fork "pulls --json \{filterString} \{show perPage} \{show page}"
  where
    filterString : String
    filterString = case filter of
                        Nothing     => "none"
                        Just Open   => "open"
                        Just Closed => "closed"

||| Grab all of the given pages of PRs and create a list of PRs from them.
list' : (filter : Maybe GitHubPRState) -> Pagination _ _ _ () -> Promise (List PullRequest)
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
           -> Promise (List PullRequest)
listOpenPRs @{config} {pageBreaks} prCount with ((finToNat prCount) `isGT` 0, pageBreaks `isGT` 0)
  _ | (No _, _) = pure empty
  _ | (_, No _) = listPullRequests config.org config.repo (Just Open) prCount
  _ | (Yes prf, Yes prf') with ((S pageBreaks) `isLTE` (finToNat prCount))
    _ | (No _)      = list' (Just Open) (metaPages  (finToNat prCount)  1)
    _ | (Yes prf'') = list' (Just Open) (metaPages' (finToNat prCount) (S pageBreaks))

||| Grab all of the given pages of PRs and create a history from them.
partition' : Pagination _ _ _ () -> Promise PRHistory
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
                  -> Promise PRHistory
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
             -> Promise (List String, List String)
listReviewers = map (.allReviewers) . (listPartitionedPRs {pageBreaks})

||| Get all of the reviews on the given PRs.
reviewsForPrs : Config => Octokit =>
                List PullRequest
             -> Promise (List Review)
reviewsForPrs prs = do
    reviewsJson <- promiseAll =<< traverse forkedReviews prs
    -- ^ list of JSON Arrays
    reviews <- either $ traverse (array parseReview) reviewsJson
    pure $ join reviews
  where
    forkedReviews : PullRequest -> Promise Future
    forkedReviews = fork . ("reviews --json " ++) . show . number

||| Get the reviews on the given PRs by the given user.
export
reviewsByUser : Config => Octokit =>
                (author : String)
             -> List PullRequest
             -> Promise (List Review)
reviewsByUser author prs = do
  let filteredPrs = filter (\pr => not $ isAuthor author pr || isRequestedReviewer author pr) prs
  -- ^ we know we aren't looking for reviews on the author's PRs.
  reviews <- reviewsForPrs filteredPrs
  pure $ filter (isAuthor author) reviews

||| Get reviews for the given PRs broken down by review author.
export
reviewsByEachUser : Config => Octokit =>
                    List PullRequest
                 -> Promise (SortedMap String (List Review))
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
                      -> Promise (SortedMap String Nat)
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
                -> Promise ()
requestReviewers @{config} pr teamNames forcedReviewers {dry} = do 
  (openReviewers, closedReviewers) <- listReviewers 100 {pageBreaks=4}
  teamMembers <- join <$> traverse (listTeamMembers config.org) teamNames

  chosenUser <- if config.assignUsers
                     then let chosenCandidates = chooseReviewers closedReviewers openReviewers teamMembers [] pr.author
                          in  randomReviewer chosenCandidates
                     else pure Nothing

  let users = (toList chosenUser) ++ forcedReviewers
  let teams = if config.assignTeams then teamNames else []
  when (not dry) $
    do ignore $ addPullReviewers config.org config.repo pr.number users teams
       when config.commentOnAssign $
         whenJust chosenUser $ \cu =>
           createComment config.org config.repo pr.number (prComment cu)
  if null users && config.assignUsers
    then putStrLn . renderString $ vsep [
                    annotate (color Yellow) $ pretty "Could not pick a user from the given Team "
                  , pretty "(perhaps the only option was the author of the pull request?)."
                  , pretty "Assigned \{teamNotice teams} to the open PR "
                  , pretty "for the current branch (\{pr.webURI})."
                  ]
    else putStrLn . renderString $ vsep [
                    pretty "Assigned \{userNotice chosenUser}\{teamNotice teams} to the open PR "
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
    prComment chosenUser = """
    :musical_note: Harmoniously assigned @\{chosenUser} to review this PR.
    """

export
identifyOrCreatePR : Config => Git => Octokit => 
                     {default False isDraft : Bool}
                  -> (branch : String) 
                  -> Promise (IdentifiedOrCreated, PullRequest)
identifyOrCreatePR @{config} {isDraft} branch = do
  [openPr] <- listPRsForBranch config.org config.repo branch
    | [] => (Created,) <$> createPR
    | _  => reject "Multiple PRs for the current brach. Harmony only handles 1 PR per branch currently."
  when (isDraft && not openPr.isDraft) $
--     putStrLn "Are you sure you want to convert the existing PR for this branch to a draft?"
    reject "There is already a PR for the current branch and Harmony does not currently support converting existing PRs to drafts."
  pure (Identified, openPr)
    where
      inlineDescription : HasIO io => io String
      inlineDescription = do
        putStrLn "What would you like the description to be (two blank lines to finish)?"
        unlines <$> getManyLines (limit 100)

      prepareDescriptionFile : HasIO io => (templateFilePath : String) -> io ()
      prepareDescriptionFile templateFilePath = do
        when !(exists templateFilePath) $
          ignore $ copyFile templateFilePath "pr_description.tmp.md"

      editorDescription : HasIO io => (editor : String) -> (templateFilePath : String) -> io (Either FileError String)
      editorDescription editor templateFilePath = do
        prepareDescriptionFile templateFilePath
        0 <- system "\{editor} pr_description.tmp.md"
          | e => pure (Left $ GenericFileError e)
        description <- assert_total $ readFile "pr_description.tmp.md" 
        --              ^ ignore the possibility that an infinte file was produced.
        when !(exists "pr_description.tmp.md") $
          ignore $ removeFile "pr_description.tmp.md"
        pure description

      continueGivenUncommittedChanges : Promise Bool
      continueGivenUncommittedChanges = do
        case !uncommittedChanges of
             Just files => do
               putStrLn "The following files have uncommitted changes:"
               putStrLn files
               yesNoPrompt "Would you like to continue creating a Pull Request anyway?"
             Nothing => pure True

      continueGivenStagedChanges : Promise Bool
      continueGivenStagedChanges = do
        case !stagedChanges of
             Just files => do
               putStrLn "The following files have staged but uncommitted changes:"
               putStrLn files
               yesNoPrompt "Would you like to continue creating a Pull Request anyway?"
             Nothing => pure True

      createPR : Promise PullRequest
      createPR = do
        -- create a remote tracking branch if needed
        whenNothing !remoteTrackingBranch $ do
          putStrLn "Creating a new remote branch..."
          pushNewBranch (fromMaybe "origin" config.defaultRemote) branch

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
        putStrLn "What branch are you merging into (ENTER for default: \{config.mainBranch})?"
        baseBranchInput <- trim <$> getLine
        let baseBranch = case strM baseBranchInput of
                              (StrCons c cs) => c `strCons` cs
                              StrNil         => config.mainBranch
        putStrLn "What would you like the title to be?"
        let titlePrefix = fromMaybe "" $ (++ " - ") <$> parseJiraPrefix branch
        putStr titlePrefix
        title <- (titlePrefix ++) . trim <$> getLine
        templateFilePath <- relativeToRoot ".github/PULL_REQUEST_TEMPLATE.md"
        description <- case config.editor of
                            Nothing => inlineDescription
                            Just ed => either (const "") id <$> editorDescription ed templateFilePath
        putStrLn "Creating PR..."
        putStrLn branch
        GitHub.createPR {isDraft} config.org config.repo branch baseBranch title description

