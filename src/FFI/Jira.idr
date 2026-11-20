module FFI.Jira

import Data.Promise
import Data.String
import Data.User
import Data.JiraIssue
import FFI
import JSON.Parser

%default total

jira_ffi : (fnName : String) -> String
jira_ffi = node_ffi "jira"

data JiraRef : Type

export
data Jira = JConfig (Ptr JiraRef)

%foreign jira_ffi "config"
prim__config : (username : String) -> (authToken : String) -> (hostname : String) -> PrimIO (Ptr JiraRef)

||| Initialize Jira (required for all other Jira FFI
||| calls).
|||
||| @authToken A GitHub PAT (personal access token)
||| @hostname  The host to use to communicate with Jira's APIs.
export
jira : (username, authToken, hostname : String) -> IO Jira
jira username authToken hostname = JConfig <$> (primIO $ prim__config username authToken hostname)

%foreign jira_ffi "list_issue_refs"
prim__listIssueRefs : Ptr JiraRef
                  -> (jql : String)
                  -> (onSuccess : String -> PrimIO ())
                  -> (onFailure : String -> PrimIO ())
                  -> PrimIO ()

||| Search for issues using the given JQL expression.
export
listIssueRefs : Jira =>
                (jql : String)
             -> Promise String (List JiraIssueRef)
listIssueRefs @{JConfig ptr} jql = do
  jsonStr <- promiseIO $ prim__listIssueRefs ptr jql
  either $ parseIssueRefsString jsonStr

namespace JQL
  export
  currentUser : String
  currentUser = "assignee = currentUser()"

  export
  unresolved : String
  unresolved = "resolution = Unresolved"

  export
  orderUpdatedDesc : String
  orderUpdatedDesc = "order by updated DESC"

||| Search for issues using the given JQL expressions (ANDed together).
export
listIssueRefs' : Jira =>
                {default JQL.orderUpdatedDesc order : String}
             -> (filters : List String)
             -> Promise String (List JiraIssueRef)
listIssueRefs' @{JConfig ptr} {order} =
  listIssueRefs . (++ " " ++ order) . joinBy " and "

export
listCurrentUserAssignedIssueRefs : Jira =>
                                   Promise String (List JiraIssueRef)
listCurrentUserAssignedIssueRefs = listIssueRefs' [currentUser, unresolved]

%foreign jira_ffi "get_issue_ref"
prim__getIssueRef : Ptr JiraRef
                 -> (issueKey : String)
                 -> (onSuccess : String -> PrimIO ())
                 -> (onFailure : String -> PrimIO ())
                 -> PrimIO ()

||| Get an issue by its key (aka slug).
export
getIssueRef : Jira =>
             (issueKey : String)
          -> Promise String JiraIssueRef
getIssueRef @{JConfig ptr} issueKey = do
  jsonStr <- promiseIO $ prim__getIssueRef ptr issueKey
  either $ parseIssueRefString jsonStr

%foreign jira_ffi "get_me"
prim__getMe : Ptr JiraRef
                -> (onSuccess : String -> PrimIO ())
                -> (onFailure : String -> PrimIO ())
                -> PrimIO ()

export
getSelf : Jira =>
          Promise String User
getSelf @{JConfig ptr} = do
  jsonStr <- promiseIO $ prim__getMe ptr
  either $ parseJiraUserString jsonStr
