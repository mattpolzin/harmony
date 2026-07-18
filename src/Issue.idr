module Issue

import Data.Config
import Data.Issue
import Data.Project
import Data.Promise
import Data.String
import Data.List

import Config

import FFI.GitHub
import System.File
import Util.Prompting

%default total

public export
data IssueCategory = Bugfix | Feature

public export
data IssueIdent = NoInfo
                | IssueTitle String
                | IssueNumber String

export
Show IssueCategory where
  show Bugfix = "Bugfix"
  show Feature = "Feature"

maybeParentIssuePrompt : Config =>
                         Octokit =>
                         Promise' (Maybe Issue)
maybeParentIssuePrompt @{config} =
  case config.defaultParentIssue of
       Nothing => pure Nothing
       Just defaultParentIssueNum => goWithIssueNum defaultParentIssueNum

  where
    clearDefaultPrompt : Issue -> Promise' ()
    clearDefaultPrompt issue = do
      putStrLn ""
      True <- yesNoPrompt {defaultAnswer = False}
                           "Do you want to clear the current default parent issue (\{show issue})?"
        | False => pure ()
      
      ignore (clearDefaultParentIssue config)

    goWithIssueNum : (issueNum : Integer) -> Promise' (Maybe Issue)
    goWithIssueNum issueNum = do
      issue <- getIssue config.org config.repo (show issueNum)
      
      True <- yesNoPrompt "Would you like your new issue to be a sub-issue of \{show issue}?"
        | False => do clearDefaultPrompt issue
                      pure Nothing
      pure $ Just issue

export
createNewIssueWithMessage : Config =>
                            Octokit =>
                            (message : String)
                         -> (baseBranchGuess : String)
                         -> (issueTitle : Maybe String)
                         -> (project : Maybe ProjectRef)
                         -> Promise' Issue
createNewIssueWithMessage @{config} message baseBranchGuess issueTitle' project = do
  putStrLn message
  putStrLn ""

  parentIssue <- maybeParentIssuePrompt

  issueTitle <-
    case issueTitle' of
         Just title => pure title
         Nothing    => do
            putStrLn "What would you like the issue title to be?"
            trim <$> getLine

  let bodyPrefix = comments

  issueBody <- case config.editor of
                    Nothing => inlineDescription issuePrompt bodyPrefix
                    Just ed => either (const "") id <$>
                                 editorDescription ed Nothing bodyPrefix

  createIssue config.org config.repo project (reference <$> parentIssue) issueTitle issueBody
    where
      issuePrompt : String
      issuePrompt = "What would you like the issue description to be (two blank lines to finish)?"

      baseBranchComment : Maybe String
      baseBranchComment =
        if baseBranchGuess /= config.mainBranch
           then Just $ Issue.baseBranchComment baseBranchGuess
           else Nothing

      closeIssueWithBranchComment : String
      closeIssueWithBranchComment = Issue.closeWithPRComment True

      comments = 
        unlines $ catMaybes
          [ baseBranchComment
          , Just closeIssueWithBranchComment
          ]

showIssueOption : Issue -> String
showIssueOption issue =
  """
      [\{show issue.number}]:
  \{issue.body}

  """

export
getIssueByTitle : Config =>
                  Octokit =>
                  (title : String)
               -> Promise' Issue
getIssueByTitle @{config} title = do
  issues <- listIssues config.org config.repo 100
  let [issue] = filter (\i => i.title == title) issues
    | [] => reject "Issue with title '\{title}' was not one of the 100 most recent issues."
    | issues => reject """
                       Issue title '\{title}' is ambiguous. You'll have to use an issue number.
                       \{concatMap showIssueOption issues}
                       """
  pure issue
