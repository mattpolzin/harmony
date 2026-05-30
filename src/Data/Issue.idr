module Data.Issue

import Data.Date
import Data.Either
import Data.List
import Data.String
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

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

baseBranchCommentPrefix : String
baseBranchCommentPrefix = "<!-- base-branch: "

export
baseBranchComment : (baseBranch : String) -> String
baseBranchComment baseBranch =
  """
  \{baseBranchCommentPrefix}\{baseBranch}
  -->
  """

export
(.baseBranchGuess) : Issue -> Maybe String
issue.baseBranchGuess = go . lines $ issue.body
  where
    getBaseFromLine : String -> Maybe String
    getBaseFromLine line =
      if baseBranchCommentPrefix `isPrefixOf` line
         then Just (pack . drop (length baseBranchCommentPrefix) $ unpack line)
         else Nothing

    go : List String -> Maybe String
    go [] = Nothing
    go (line :: lines) = getBaseFromLine line <|> go lines

parseDateTime : String -> Either String Date
parseDateTime = maybeToEither "Failed to parse Date" . parseDateTimeString

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

||| Parse a single user from a JSON String
export
parseIssueString : String -> Either String Issue
parseIssueString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseIssue

||| Parse a list of issues from a JSON String
export
parseIssuesString : String -> Either String (List Issue)
parseIssuesString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parseIssue

