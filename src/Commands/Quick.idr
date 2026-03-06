module Commands.Quick

import Data.Config
import Data.Issue
import Data.List
import Data.Promise
import Data.String

import FFI.GitHub
import Util

import System.Git
import System.File

%default total

public export
data IssueCategory = Bugfix | Feature

Show IssueCategory where
  show Bugfix = "Bugfix"
  show Feature = "Feature"

dasherize : String -> String
dasherize = pack . replaceOn ' ' '-' . unpack

branchNameSuggestion : String -> String
branchNameSuggestion = toLower . dasherize

createNewIssue : Config =>
                 Octokit =>
                 (issueTitle : Maybe String)
              -> Promise' Issue
createNewIssue @{config} issueTitle' = do
  putStrLn "Creating a new GitHub issue and branch."
  putStrLn ""

  issueTitle <-
    case issueTitle' of
         Just title => pure title
         Nothing    => do
            putStrLn "What would you like the issue title to be?"
            trim <$> getLine

  issueBody <- case config.editor of
                    Nothing => inlineDescription issuePrompt ""
                    Just ed => either (const "") id <$>
                                 editorDescription ed Nothing ""

  createIssue config.org config.repo issueTitle issueBody
    where
      issuePrompt : String
      issuePrompt = "What would you like the issue description to be (two blank lines to finish)?"

public export
data IssueIdent = NoInfo
                | IssueTitle String
                | IssueNumber String

||| Quickly create a branch to go along with a new GitHub or existing issue.
export
quickStartNewWork : Config =>
                    Octokit =>
                    IssueCategory
                 -> (issueIdent : IssueIdent)
                 -> Promise' ()
quickStartNewWork @{config} issueCategory issueIdent = do
  issue <- case issueIdent of
                NoInfo                 => createNewIssue Nothing
                IssueTitle  issueTitle => createNewIssue (Just issueTitle)
                IssueNumber issueNum   => getIssue config.org config.repo issueNum

  let branchTemplate = "\{branchPrefix}/\{show issue.number}/"
  let defaultBranchSlug = branchNameSuggestion issue.title
  let defaultBranchName = branchTemplate ++ defaultBranchSlug
  putStrLn "What would you like the branch to be named? \{enterForDefaultStr defaultBranchName}"
  putStr branchTemplate
  branchSlug <- orIfEmpty (Just defaultBranchSlug) . trim <$> getLine
  
  checkoutBranch {b=New} "\{branchTemplate}\{branchSlug}"

  where
    branchPrefix : String
    branchPrefix = toLower $ show issueCategory

