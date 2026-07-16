module Issue

import Data.Config
import Data.Issue
import Data.Project
import Data.Promise
import Data.String

import FFI.GitHub
import System.File
import Util.Prompting

%default total

public export
data IssueCategory = Bugfix | Feature

export
Show IssueCategory where
  show Bugfix = "Bugfix"
  show Feature = "Feature"

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

  createIssue config.org config.repo project Nothing issueTitle issueBody
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
