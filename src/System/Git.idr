module System.Git

import Data.Promise
import Data.String

import Util.System

import Language.Reflection
import Derive.Prelude

%language ElabReflection

%default total

public export
data BranchStatus = Existing | New

%runElab derive `{BranchStatus} [Eq]

git : HasIO io => List String -> io (String, String, Int)
git args = assert_total $ run' ("git" :: args)

stdout : (String, String, Int) -> String
stdout = fst

mapStdout : (String -> a) -> (String, String, Int) -> (a, String, Int)
mapStdout = mapFst

||| Drop the first char if it fits the predicate.
dropFirstIf : (Char -> Bool) -> String -> String
dropFirstIf pred str with (asList str)
  dropFirstIf pred "" | [] = ""
  dropFirstIf pred str@(strCons c str') | (c :: _) =
    if pred c then str' else str

promise : IO (a, String, Int) -> Promise' a
promise gitOp = liftIO res >>= either
  where
    res : IO (Either String a)
    res =  do
      (stdout, stderr, exitCode) <- gitOp
      case exitCode of
           0 => pure $ Right stdout
           _ => pure $ Left stderr

export
currentBranch : Promise' String
currentBranch = map trim . promise $ git ["branch", "--show-current"]

parseBranchList : String -> List String
parseBranchList = map (trim . dropFirstIf (== '*')) . lines

export
listBranches : Promise' (List String)
listBranches = 
  promise $
    mapStdout parseBranchList <$> (git ["branch", "--list"])

export
listBranches' : HasIO io => io (List String)
listBranches' = 
  parseBranchList . stdout <$> (git ["branch", "--list"])

export
checkoutBranch : {default Existing b : BranchStatus} -> (branch : String) -> Promise' ()
checkoutBranch {b=branchStatus} branch = 
  let constantArgs = [branch]
      args = case branchStatus of
                  New => ("-b" :: constantArgs)
                  Existing => constantArgs
  in ignore . promise $ git ("checkout" :: args)

export
pushNewBranch : (remoteName : String) -> (branch : String) -> Promise' ()
pushNewBranch remoteName branch =
  ignore . promise $ git ["push", "--set-upstream", remoteName, branch]

export
push : Promise' ()
push = ignore . promise $ git ["push"]

export
listRemotes : Promise' (List String)
listRemotes = lines . trim <$> (promise $ git ["remote"])

||| Get the Git remote URI for the remote with the given name.
||| For example,
|||   "git@github.com:org/reponame.git"
|||   "https://github.com/org/reponame.git"
export
remoteURI : (remoteName : String) -> Promise' String
remoteURI remoteName = map trim . promise $ git ["remote", "get-url", remoteName]

export
remoteTrackingBranch : Promise' (Maybe String)
remoteTrackingBranch = do
  headRef <-
    trim <$>
      (promise $ git ["symbolic-ref", "-q", "HEAD"])
  remoteBranch <-
    trim <$>
      (promise $ git [ "for-each-ref"
                     , "--format"
                     , "%(upstream:short)"
                     , headRef
                     ])
  pure $
    case strM remoteBranch of
         StrNil         => Nothing
         (StrCons _ _) => Just remoteBranch

||| Get the Git output for filenames with uncommitted changes. If there
||| are no files with uncommitted changes, returns @Nothing@.
export
uncommittedChanges : Promise' (Maybe String)
uncommittedChanges = do
  changedFiles <- map trim . promise $ git ["diff", "--name-only"]
  pure $
    case strM changedFiles of
         StrNil         => Nothing
         (StrCons _ _) => Just changedFiles

||| Get the Git output for filenames with staged changes. If there
||| are no files with staged changes, returns @Nothing@.
export
stagedChanges : Promise' (Maybe String)
stagedChanges = do
  stagedFiles <- map trim . promise $ git ["diff", "--staged", "--name-only"]
  pure $
    case strM stagedFiles of
         StrNil         => Nothing
         (StrCons x xs) => Just stagedFiles

||| Get the Git output for unpushed commits (multiple lines per
||| commit including the ref, author, and commit message). If there
||| are no unpushed commits, returns @Nothing@.
export
unpushedCommits : Promise' (Maybe String)
unpushedCommits = do
  commits <- map trim . promise $ git ["log", "@{push}.."]
  pure $
    case strM commits of
         StrNil         => Nothing
         (StrCons x xs) => Just commits

export
userEmail : Promise' String
userEmail =
  map trim . promise $ git ["config", "--get", "user.email"]

||| Get the absolute path of the Git repository's root directory
||| (the location of the `.git` folder).
export
rootDir : Promise' String
rootDir = map trim . promise $ git ["rev-parse", "--show-toplevel"]

