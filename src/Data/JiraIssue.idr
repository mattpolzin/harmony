module Data.JiraIssue

import Data.Date
import Data.Issue
import Data.Either
import Data.List
import Data.String
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

import Util.String

%default total

public export
record JiraIssueRef where
  constructor MkIssueRef
  ||| The issue's id
  id  : String
  ||| The issue's key (aka slug)
  key : String
  ||| The issue's summary (aka title)
  summary : String

export
IssueRef JiraIssueRef where
  identifier = key
  title = summary

export
Show JiraIssueRef where
  show (MkIssueRef id key summary) = "[\{key}] \{summary}"

export
parseIssueRef : JSON -> Either String JiraIssueRef
parseIssueRef json =
 do issueRef <- object json
    [id, key, fields] <- lookupAll ["id", "key", "fields"] issueRef
    id      <- string id
    key     <- string key
    summary <- ((\[e] => string e) <=< lookupAll ["summary"] <=< object) fields
    pure $ MkIssueRef {
        id
      , key
      , summary
      }

||| Parse a single issue refs from a JSON String
export
parseIssueRefString : String -> Either String JiraIssueRef
parseIssueRefString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseIssueRef

||| Parse a list of issues refs from a JSON String
export
parseIssueRefsString : String -> Either String (List JiraIssueRef)
parseIssueRefsString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parseIssueRef

export
json : JiraIssueRef -> JSON
json (MkIssueRef id key summary) = JObject [("id", JString id), ("key", JString key), ("fields", JObject [("summary", JString summary)])]
