module FFI.Jira

import Data.Promise
import Data.User
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

%foreign jira_ffi "list_issues"
prim__listIssues : Ptr JiraRef
                -> (jql : String)
                -> (onSuccess : String -> PrimIO ())
                -> (onFailure : String -> PrimIO ())
                -> PrimIO ()

export
listIssues : Jira =>
             (jql : String)
          -> Promise String String
listIssues @{JConfig ptr} jql = do
  r <- promiseIO $ prim__listIssues ptr jql
  pure r

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
