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

getManyLines : HasIO io => Fuel -> io (List String)
getManyLines = getMoreLines []
  where
    getMoreLines : (acc : List String) -> Fuel -> io (List String)
    getMoreLines acc Dry = pure (reverse acc)
    getMoreLines acc (More fuel) =
      do line <- trim <$> getLine
         -- stop collecting lines on second blank line.
         case (acc, line) of
              ("" :: rest, "") => pure (reverse rest)
              _                => getMoreLines (line :: acc) fuel

parseJiraPrefix : String -> Maybe String
parseJiraPrefix = map (pack . reverse) . guardSuccess . foldl go startOver . unpack
  where
    data Part = Start | Proj | Dash | Num | End

    startOver : (Part, List Char)
    startOver = (Start, [])

    guardSuccess : (Part, List Char) -> Maybe (List Char)
    guardSuccess (Num, y) = Just y
    guardSuccess (End, y) = Just y
    guardSuccess _ = Nothing

    go : (Part, List Char) -> Char -> (Part, List Char)
      -- start off looking for alpha characters that are a Jira Project slug.
    go (Start, cs) c   = if isAlpha c then (Proj, c :: cs) else startOver

      -- if you've found alpha characters, keep an eye out for a dash.
    go (Proj , cs) '-' = (Dash, '-' :: cs)

      -- continue parsing alpha until you find the aforementioned dash.
      -- start over if you find something else.
    go (Proj , cs) c   = if isAlpha c then (Proj, c :: cs) else startOver

      -- we expect a number after a dash or else we start over.
    go (Dash , cs) c   = if isDigit c then (Num, c :: cs) else startOver

      -- now we expect numbers until we reach the end of the prefix.
    go (Num  , cs) c   = if isDigit c then (Num, c :: cs) else (End, cs)

      -- once we are done, we just ignore the remaining characters.
    go (End  , cs) c   = (End, cs)

public export
data IdentifiedOrCreated = Identified | Created

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
  do closedReviewers <- listPullReviewers config.org config.repo (Just Closed) 30
     -- printLn closedReviewers
     openReviewers   <- listPullReviewers config.org config.repo (Just Open) 40
     -- printLn openReviewers
     teamMembers     <- join <$> traverse (listTeamMembers config.org) teamNames
     -- printLn teamMembers
     let chosenCandidates = chooseReviewers closedReviewers openReviewers teamMembers [] pr.author
     -- printLn chosenCandidates
     chosenUser <- randomReviewer chosenCandidates
     let users = (toList chosenUser) ++ forcedReviewers
     when (not dry) $
       ignore $ addPullReviewers config.org config.repo pr.number users teamNames
     if null users
        then putStrLn """
                      Could not pick a user from the given Team 
                      (perhaps the only option was the author of the pull request?).
                      """
        else putStrLn """
                      Assigned \{userNotice chosenUser}\{teamNotice} to the open PR 
                      for the current branch (\{pr.webURI}).
                      """
  where
    csv : List String -> String
    csv = join ", "

    userNotice : (chosenReviewer : Maybe String) -> String
    userNotice Nothing       = case forcedReviewers of
                                    []         => "no users"
                                    reviewers  => "\{csv reviewers}"
    userNotice (Just chosen) = "\{csv (chosen :: forcedReviewers)}"

    teamNotice : String
    teamNotice = case teamNames of 
                      []     => ""
                      [name] => " and team \{name}"
                      names  => " and teams \{csv names}" 

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

