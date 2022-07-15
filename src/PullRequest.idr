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
import Data.String
import Data.String.Extra
import FFI.Concurrency
import FFI.Git
import FFI.GitHub
import Language.JSON
import Language.JSON.Accessors
import Reviewer
import System.Node
import System.File
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Util
import System.File.Node

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
forkedPRs : (perPage : Nat) -> (currentPageIdx : Nat) -> (currentPageSize : Nat) -> (x : ()) -> Promise Future
forkedPRs perPage page _ _ = fork "pulls --json \{show perPage} \{show page}"

||| Grab all of the given pages of PRs and create a history from them.
partition' : Pagination _ _ _ () -> Promise PRHistory
partition' pgs =
  do prJsons <- promiseAll =<< traverse' forkedPRs pgs
     pulls   <- either $ traverse (array parsePR) prJsons
     pure $ partition (join pulls)

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
    _ | (No _)       = partition' (metaPages  (finToNat prCount)  1)
    _ | (Yes prf'') = partition' (metaPages' (finToNat prCount) (S pageBreaks))

export
listReviewers : Config => Octokit =>
                {default 0 pageBreaks : Nat}
             -> (prCount : Fin 101)
             -> Promise (List String, List String)
listReviewers = map (.allReviewers) . (listPartitionedPRs {pageBreaks})

||| Get the reviews on the given PRs by the given user.
export
reviewsForUser : Config => Octokit =>
                 (author : String)
              -> List PullRequest
              -> Promise (List Review)
reviewsForUser @{config} author prs =
  do let filteredPrs = filter (\pr => not $ isAuthor author pr || isRequestedReviewer author pr) prs
     -- ^ we know we aren't looking for reviews on the author's PRs.
     reviewsJson <- promiseAll =<< traverse forkedReviews filteredPrs
     -- ^ list of JSON Arrays
     reviews <- either $ traverse (array parseReview) reviewsJson
     pure $ filter (isAuthor author) (join reviews)
  where
    forkedReviews : PullRequest -> Promise Future
    forkedReviews = fork . ("reviews --json " ++) . show . number

||| Request reviews.
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
requestReviewers @{config} pr teamNames forcedReviewers {dry} =
  do (openReviewers, closedReviewers) <- listReviewers 100 {pageBreaks=4}
     teamMembers <- join <$> traverse (listTeamMembers config.org) teamNames
     -- printLn teamMembers
     let chosenCandidates = chooseReviewers closedReviewers openReviewers teamMembers [] pr.author
     -- printLn chosenCandidates
     chosenUser <- randomReviewer chosenCandidates
     let users = (toList chosenUser) ++ forcedReviewers
     let teams = if config.assignTeams then teamNames else []
     when (not dry) $
       do ignore $ addPullReviewers config.org config.repo pr.number users teams
          when config.commentOnAssign $
            whenJust chosenUser $ \cu =>
              createComment config.org config.repo pr.number (prComment cu)
     if null users
       then putStrLn . renderString $ vsep [
                       annotate (color Yellow) $ pretty "Could not pick a user from the given Team "
                     , pretty "(perhaps the only option was the author of the pull request?)."
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
                     (branch : String) 
                  -> Promise (IdentifiedOrCreated, PullRequest)
identifyOrCreatePR @{config} branch =
  do [openPr] <- listPRsForBranch config.org config.repo branch
       | [] => (Created,) <$> createPR
       | _  => reject "Multiple PRs for the current brach. We only handle 1 PR per branch currently."
     pure (Identified, openPr)
  where
    inlineDescription : HasIO io => io String
    inlineDescription = do
      putStrLn "What would you like the description to be (two blank lines to finish)?"
      unlines <$> getManyLines (limit 100)

    prepareDescriptionFile : HasIO io => io ()
    prepareDescriptionFile = do
      when !(exists ".github/PULL_REQUEST_TEMPLATE.md") $
        ignore $ copyFile ".github/PULL_REQUEST_TEMPLATE.md" "pr_description.tmp.md"

    editorDescription : HasIO io => (editor : String) -> io (Either FileError String)
    editorDescription editor = do
      prepareDescriptionFile
      0 <- system "\{editor} pr_description.tmp.md"
        | e => pure (Left $ GenericFileError e)
      description <- assert_total $ readFile "pr_description.tmp.md" 
      --              ^ ignore the possibility that an infinte file was produced.
      when !(exists "pr_description.tmp.md") $
        Node.removeFile "pr_description.tmp.md"
      pure description

    createPR : Promise PullRequest
    createPR = do
      when (!remoteTrackingBranch == Nothing) $
        do -- TODO: Don't assume origin. we can get that from git. store in config?
           putStrLn "Creating a new remote branch..."
           pushNewBranch "origin" branch
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
      description <- case config.editor of
                          Nothing => inlineDescription
                          Just ed => either (const "") id <$> editorDescription ed
      putStrLn "Creating PR..."
      putStrLn branch
      GitHub.createPR config.org config.repo branch baseBranch title description

