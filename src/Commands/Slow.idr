module Commands.Slow

import Data.Config
import Data.Issue
import Data.List
import Data.Promise
import Data.String

import Config
import System.File
import Util
import Util.Prompting

import FFI.GitHub

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Util

%default total

removeBlankLine : String -> Maybe String
removeBlankLine "" = Nothing
removeBlankLine str = Just str

removeComment : String -> Maybe String
removeComment str =
  if "#" `isPrefixOf` str
     then Nothing
     else Just str

getIssueTitleFromLines : List String -> List String
getIssueTitleFromLines = mapMaybe (removeComment <=< removeBlankLine . trim)

||| Set an issue to be the parent of issues to come and optionally stub some
||| child issues out now.
export
slowStartWork : Config =>
                Octokit =>
                (issue: Issue)
             -> {default False stubIssueOut : Bool}
             -> Promise' ()
slowStartWork @{config} issue {stubIssueOut} = do
  -- we set the default parent issue in the config so that future calls to
  -- `quick` will create issues under this project.
  renderIO $
    reflow """
           Issue \{show issue.number} will be the default parent of new issues until
           you call `slow` again or clear it with `config defaultParentIssue none`.
           """
  ignore $ setDefaultParentIssue config issue.reference

  let True = stubIssueOut
    | False => putStrLn """

                        You can create multiple sub-issues at once with `slow --stub`.

                        You can create a sub-new issue and begin work on it with `quick`.
                        """

  stubbedIssueTitles <-
    case config.editor of
         Nothing => inlineDescription inlineDescriptionPrompt ""
         Just ed => either (const "") id <$>
                      editorDescription ed Nothing instructions {tmpFileExtension="sh"}

  let issueTitles@(_ :: _) = getIssueTitleFromLines $ lines stubbedIssueTitles
    | [] => putStrLn "Alright, no issues were stubbed."

  let numIssues = length issueTitles

  putStrLn ""
  putStr "Createing \{issueNumLabel numIssues}"
  issues <- for issueTitles $ \title => do
    putStr . renderString $ annotate (color Green) "."
    createIssue config.org
                config.repo
                config.defaultProject
                (Just issue.reference)
                title
                ""
  
  putStrLn ""
  putStrLn "The following issues were created:"
  renderIO $
    vsep (renderIssue <$> issues)

  where
    issueNumLabel : Nat -> String
    issueNumLabel 1 = "1 issue"
    issueNumLabel n = "\{show n} issues"

    inlineDescriptionPrompt : String
    inlineDescriptionPrompt =
        "Enter one issue title per line (two blank lines to finish)"

    instructions : String
    instructions = """

                   # Enter one issue title per line. Blanks and lines starting with '#' are ignored.

                   """

    renderIssue : Issue -> Doc AnsiStyle
    renderIssue issue = issueNum <++> pretty issue.title

      where
        issueNum : Doc AnsiStyle
        issueNum =
          enclose "[" "]" (annotate (color Green) . pretty $ show issue.number)

