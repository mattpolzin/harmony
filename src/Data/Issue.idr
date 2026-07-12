module Data.Issue

import Data.Date
import Data.Either
import Data.List
import Data.String
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

import Util.String

%default total

public export
record Issue where
  constructor MkIssue
  ||| The issue's "number" (as seen in URIs referring to the issue).
  number    : Integer
  ||| The issue's title
  title     : String
  ||| The issue's body
  body      : String
  ||| When the issue was created.
  createdAt : Date
  ||| The `login` of the author of the issue.
  author    : String
  ||| The `login` of the assignee of the issue.
  assignee  : Maybe String
  ||| The number of PRs linked to the issue. This is not returned for all
  ||| queries; if not available then it is `Nothing`.
  linkedPRCount : Maybe Nat

%name Issue issue, issue1, issue2

export
Show Issue where
  show (MkIssue number title _ _ author _ _) = 
    "[\{show number}] \{authorString} - \{title}"
    where
      authorString : String
      authorString = padRight 15 ' ' $ show author

export
isAuthor : String -> Issue -> Bool
isAuthor login = (== login) . author

export
isAssignee : String -> Issue -> Bool
isAssignee login (MkIssue _ _ _ _ _ Nothing _) = False
isAssignee login (MkIssue _ _ _ _ _ (Just assignee) _) = login == assignee

-- the beginning and end tags of the html comment are intentionally on their
-- given lines here to make parsing as low overhead as possible.
baseBranchCommentPrefix : String
baseBranchCommentPrefix = "<!-- base-branch: "

export
baseBranchComment : (baseBranch : String) -> String
baseBranchComment baseBranch =
  """
  \{baseBranchCommentPrefix}\{baseBranch}
  -->
  """

-- the beginning and end tags of the html comment are intentionally on their
-- given lines here to make parsing as low overhead as possible.
closeIssueWithPRCommentPrefix : String
closeIssueWithPRCommentPrefix = "<!-- close-with-pr: "

export
closeWithPRComment : Bool -> String
closeWithPRComment enabled =
  """
  \{closeIssueWithPRCommentPrefix}\{toString enabled}
  -->
  """

  where
    toString : Bool -> String
    toString False = "no"
    toString True = "yes"

public export
record IssueCommentConfig where
  constructor MkIssueCommentConfig
  baseBranchGuess : Maybe String
  closeWithPr : Maybe Bool

empty : IssueCommentConfig
empty = MkIssueCommentConfig Nothing Nothing

export
commentConfig : Issue -> IssueCommentConfig
commentConfig issue = foldr go empty . lines $ issue.body
  where
    getBaseFromLine : String -> IssueCommentConfig ->  IssueCommentConfig
    getBaseFromLine line cfg =
      if baseBranchCommentPrefix `isPrefixOf` line
         then { baseBranchGuess := Just (pack . drop (length baseBranchCommentPrefix) $ unpack line) } cfg
         else cfg

    getCloseWithPRFromLine : String -> IssueCommentConfig ->  IssueCommentConfig
    getCloseWithPRFromLine line cfg =
      if closeIssueWithPRCommentPrefix `isPrefixOf` line
         then { closeWithPr := (parseBool . pack . drop (length closeIssueWithPRCommentPrefix) $ unpack line) } cfg
         else cfg

    go : String -> IssueCommentConfig -> IssueCommentConfig
    go line =
      getBaseFromLine line . getCloseWithPRFromLine line

public export
record ConfiguredIssue where
  constructor MkConfiguredIssue
  issue : Issue
  config : IssueCommentConfig

export
(.baseBranchGuess) : ConfiguredIssue -> Maybe String
i.baseBranchGuess = i.config.baseBranchGuess

export
(.closeWithPr) : ConfiguredIssue -> Maybe Bool
i.closeWithPr = i.config.closeWithPr

export
(.title) : ConfiguredIssue -> String
i.title = i.issue.title

export
(.number) : ConfiguredIssue -> Integer
i.number = i.issue.number

export
configuredIssue : Issue -> ConfiguredIssue
configuredIssue issue =
  MkConfiguredIssue issue (commentConfig issue)

export
parseIssue : JSON -> Either String Issue
parseIssue json =
 do issue <- object json
    [issueNumber, issueTitle, issueBody, authorLogin, createdAtStr, assigneeLogin] <- lookupAll ["issue_number", "title", "body", "author", "created_at", "assignee"] issue
    number      <- integer issueNumber
    title       <- string issueTitle
    body        <- maybe "" id <$> optional string issueBody
    author      <- string authorLogin
    createdAt   <- parseDateTime =<< string createdAtStr
    assignee    <- optional string assigneeLogin
    linkedPRCount <- maybe (Right Nothing) 
                           (optional nat)
                           (lookup "linked_pr_count" issue)
    pure $ MkIssue {
        number
      , title
      , body
      , createdAt
      , author
      , assignee
      , linkedPRCount
      }

||| Parse a single issue from a JSON String
export
parseIssueString : String -> Either String Issue
parseIssueString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseIssue

||| Parse a list of issues from a JSON String
export
parseIssuesString : String -> Either String (List Issue)
parseIssuesString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parseIssue

