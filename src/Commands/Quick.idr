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

namespace TestDasherize
  replacesSpacesWithDashes : dasherize "a b c" === "a-b-c"
  replacesSpacesWithDashes = Refl

||| Quickly create a new GitHub issue and branch to go along with it.
export
quickStartNewWork : Config =>
                    Octokit =>
                    IssueCategory
                 -> (issueTitle: Maybe String)
                 -> Promise' ()
quickStartNewWork @{config} issueCategory issueTitle' = do
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

  issue <- createIssue config.org config.repo issueTitle issueBody

  let branchTemplate = "\{branchPrefix}/\{show issue.number}/"
  let defaultBranchSlug = dasherize issueTitle
  let defaultBranchName = branchTemplate ++ defaultBranchSlug
  putStrLn "What would you like the branch to be named? \{enterForDefaultStr defaultBranchName}"
  putStr branchTemplate
  branchSlug <- orIfEmpty (Just defaultBranchSlug) . trim <$> getLine
  
  checkoutBranch {b=New} "\{branchTemplate}\{branchSlug}"

  where
    issuePrompt : String
    issuePrompt = "What would you like the issue description to be (two blank lines to finish)?"

    branchPrefix : String
    branchPrefix = toLower $ show issueCategory

