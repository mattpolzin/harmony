module Commands.Quick

import Data.Config
import Data.Promise
import Data.String
import Data.Issue

import FFI.Git
import FFI.GitHub
import Util

import System.File

||| Quickly create a new GitHub issue and branch to go along with it.
export
quickStartNewWork : Config =>
                    Git =>
                    Octokit =>
                    Promise' ()
quickStartNewWork @{config} = do
  putStrLn "Creating a new GitHub issue and branch."
  putStrLn ""

  putStrLn "What would you like the issue title to be?"
  issueTitle <- trim <$> getLine

  issueBody <- case config.editor of
                    Nothing => inlineDescription issuePrompt ""
                    Just ed => either (const "") id <$>
                                 editorDescription ed Nothing ""

  issue <- createIssue config.org config.repo issueTitle issueBody

  putStrLn "What would you like the branch to be named?"
  putStr "feature/\{show issue.number}/"
  branchSlug <- trim <$> getLine
  
  checkoutBranch {b=New} "feature/\{show issue.number}/\{branchSlug}"

  where
    issuePrompt : String
    issuePrompt = "What would you like the issue description to be (two blank lines to finish)?"
