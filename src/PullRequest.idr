module PullRequest

import Data.Config
import Data.Fuel
import Data.List
import Data.List1
import Data.Promise
import Data.PullRequest
import Data.String
import Data.String.Extra
import FFI.GitHub
import Reviewer
import Text.PrettyPrint.PrettyPrinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Util

public export
data IdentifiedOrCreated = Identified | Created

export
listReviewers : Config => Octokit =>
                (openCount : Fin 100)
             -> (closedCount : Fin 100)
             -> Promise (List String, List String)
listReviewers @{config} openCount closedCount =
  do openReviewers   <- listPullReviewers config.org config.repo (Just Open) openCount
     -- printLn openReviewers
     closedReviewers <- listPullReviewers config.org config.repo (Just Closed) closedCount
     -- printLn closedReviewers
     pure (openReviewers, closedReviewers)

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
  do (openReviewers, closedReviewers) <- listReviewers 40 30
     teamMembers     <- join <$> traverse (listTeamMembers config.org) teamNames
     -- printLn teamMembers
     let chosenCandidates = chooseReviewers closedReviewers openReviewers teamMembers [] pr.author
     -- printLn chosenCandidates
     chosenUser <- randomReviewer chosenCandidates
     let users = (toList chosenUser) ++ forcedReviewers
     let teams = if config.assignTeams then teamNames else []
     when (not dry) $
       ignore $ addPullReviewers config.org config.repo pr.number users teams
     liftIO $ 
       if null users
         then putStrLn . maybeDecorate $ vsep [
                         annotate (color Yellow) $ pretty "Could not pick a user from the given Team "
                       , pretty "(perhaps the only option was the author of the pull request?)."
                       ]
         else putStrLn . maybeDecorate $ vsep [
                         pretty "Assigned \{userNotice chosenUser}\{teamNotice teams} to the open PR "
                       , pretty "for the current branch (\{pr.webURI})."
                       ]
  where
    maybeDecorate : Doc AnsiStyle -> String
    maybeDecorate doc =
      let render = if config.colors then id else unAnnotate
      in  renderString . layoutPretty defaultLayoutOptions $ render doc

    csv : List String -> String
    csv = maybeDecorate . encloseSep emptyDoc emptyDoc (pretty ", ") . map (annotate (color Green) . pretty)

    userNotice : (chosenReviewer : Maybe String) -> String
    userNotice Nothing       = case forcedReviewers of
                                    []         => "no users"
                                    reviewers  => "\{csv reviewers}"
    userNotice (Just chosen) = "\{csv (chosen :: forcedReviewers)}"

    teamNotice : List String -> String
    teamNotice []     = ""
    teamNotice [name] = " and team \{csv [name]}"
    teamNotice names  = " and teams \{csv names}"

export
identifyOrCreatePR : Config => Octokit => 
                     (branch : String) 
                  -> Promise (IdentifiedOrCreated, PullRequest)
identifyOrCreatePR @{config} branch =
  do [openPr] <- listPRsForBranch config.org config.repo branch
       | [] => (Created,) <$> createPR
       | _  => reject "Multiple PRs for the current brach. We only handle 1 PR per branch currently."
     pure (Identified, openPr)
  where
    createPR : Promise PullRequest
    createPR =
      do putStrLn "Creating a new PR for the current branch (\{branch})."
         putStrLn "What branch are you merging into (ENTER for default: \{config.mainBranch})?"
         baseBranchInput <- trim <$> getLine
         let baseBranch = case strM baseBranchInput of
                               (StrCons c cs) => c `strCons` cs
                               StrNil         => config.mainBranch
         putStrLn "What would you like the title to be?"
         let titlePrefix = fromMaybe "" $ (++ " - ") <$> parseJiraPrefix branch
         putStr titlePrefix
         title <- (titlePrefix ++) . trim <$> getLine
         putStrLn "What would you like the description to be (two blank lines to finish)?"
         description <- unlines <$> getManyLines (limit 100)
         putStrLn "Creating PR..."
         GitHub.createPR config.org config.repo branch baseBranch title description

