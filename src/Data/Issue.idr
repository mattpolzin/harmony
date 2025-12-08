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

%name Issue issue, issue1, issue2

export
Show Issue where
  show (MkIssue number title _ _ author _) = 
    "[\{show number}] \{authorString} - \{title}"
    where
      authorString : String
      authorString = padRight 15 ' ' $ show author

export
isAuthor : String -> Issue -> Bool
isAuthor login = (== login) . author

export
isAssignee : String -> Issue -> Bool
isAssignee login (MkIssue _ _ _ _ _ Nothing) = False
isAssignee login (MkIssue _ _ _ _ _ (Just assignee)) = login == assignee

parseDateTime : String -> Either String Date
parseDateTime = maybeToEither "Failed to parse Date" . parseDateTimeString

export
parseIssue : JSON -> Either String Issue
parseIssue json =
 do issue <- object json
    [issueNumber, issueTitle, issueBody, authorLogin, createdAtStr, assigneeLogin] <- lookupAll ["issue_number", "title", "body", "author", "created_at", "assignee"] issue
    number      <- integer issueNumber
    title       <- string issueTitle
    body        <- string issueBody
    author      <- string authorLogin
    createdAt   <- parseDateTime =<< string createdAtStr
    assignee    <- optional string assigneeLogin
    pure $ MkIssue {
        number
      , title
      , body
      , createdAt
      , author
      , assignee
      }

||| Parse a single user from a JSON String
export
parseIssueString : String -> Either String Issue
parseIssueString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseIssue

||| Parse a list of users from a JSON String
export
parseIssuesString : String -> Either String (List Issue)
parseIssuesString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parseIssue

