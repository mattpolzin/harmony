module Commands.Quick

import Data.Config
import Data.Issue
import Data.List
import Data.Promise
import Data.String
import public Data.Project

import FFI.GitHub
import Util

import ShellCompletion.Util

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

||| In order to support tab completion of multi-word project titles, spaces
||| have been turned into another character to "slugify" the labels. Still, it
||| is possible the user has entered a project title that literally contains
||| the character used during slugification, so to unslugify, we first see if a
||| project appears in the configured list of projects. If it does then we use
||| it exactly but if it doesn't then we unslugify it before using it.
export
projectFromUnsluggifiedTitle : (configProjectRefs : List ProjectRef)
                            -> (slugifiedTitle : String)
                            -> Maybe ProjectRef
projectFromUnsluggifiedTitle configProjects slugifiedTitle =
  case findByTitle slugifiedTitle of
       Just project => Just project
       Nothing      => findByTitle $ unslugify slugifiedTitle
  where
    findByTitle : String -> Maybe ProjectRef
    findByTitle title = find (\p => (p.title == title)) configProjects 

branchNameSuggestion : String -> String
branchNameSuggestion = toLower . dasherize

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

  let bodyPrefix = baseBranchComment

  issueBody <- case config.editor of
                    Nothing => inlineDescription issuePrompt bodyPrefix
                    Just ed => either (const "") id <$>
                                 editorDescription ed Nothing bodyPrefix

  createIssue config.org config.repo project issueTitle issueBody
    where
      issuePrompt : String
      issuePrompt = "What would you like the issue description to be (two blank lines to finish)?"

      baseBranchComment : String
      baseBranchComment =
        if baseBranchGuess /= config.mainBranch
           then """
                \{Issue.baseBranchComment baseBranchGuess}

                """
           else ""

export
createNewIssue : Config =>
                 Octokit =>
                 (baseBranchGuess : String)
              -> (issueTitle : Maybe String)
              -> (project : Maybe ProjectRef)
              -> Promise' Issue
createNewIssue = createNewIssueWithMessage "Creating a new GitHub issue and branch."

public export
data IssueIdent = NoInfo
                | IssueTitle String
                | IssueNumber String

||| Quickly create a branch to go along with a new or existing GitHub issue.
export
quickStartNewWork : Config =>
                    Octokit =>
                    IssueCategory
                 -> (issueIdent : IssueIdent)
                 -> {default Nothing project : Maybe ProjectRef}
                 -> Promise' ()
quickStartNewWork @{config} issueCategory issueIdent {project} = do
  -- We guess that the base branch is possibly the branch
  -- checked out when the new issue is being created.
  baseBranchGuess <- currentBranch
  let createIssue = createNewIssue baseBranchGuess

  issue <- case issueIdent of
                NoInfo                 => createIssue Nothing project
                IssueTitle  issueTitle => createIssue (Just issueTitle) project
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
