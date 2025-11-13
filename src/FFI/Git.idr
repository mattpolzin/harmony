module FFI.Git

import Data.String
import Data.Promise
import FFI

import Language.Reflection
import Derive.Prelude

%language ElabReflection

%default total

git_ffi : (fnName : String) -> String
git_ffi = node_ffi "git"

data GitRef : Type

export
data Git = G (Ptr GitRef)

%foreign git_ffi "git"
prim__git : PrimIO (Ptr GitRef)

export
git : HasIO io => io Git
git = G <$> (primIO $ prim__git)

%foreign git_ffi "current_branch"
prim__currentBranch : Ptr GitRef
                   -> (onSuccess : String -> PrimIO ())
                   -> (onFailure : String -> PrimIO ())
                   -> PrimIO ()

export
currentBranch : Git => Promise' String
currentBranch @{G ptr} = promiseIO $ prim__currentBranch ptr

%foreign git_ffi "list_branches"
prim__listBranches : Ptr GitRef
                 -> (onSuccess : String -> PrimIO ())
                 -> (onFailure : String -> PrimIO ())
                 -> PrimIO ()

export
listBranches : Git => Promise' (List String)
listBranches @{G ptr} = lines <$> (promiseIO $ prim__listBranches ptr)

%foreign git_ffi "list_branches_sync"
prim__listBranchesSync : Ptr GitRef
                      -> PrimIO String

export
listBranchesSync : Git => HasIO io => io (List String)
listBranchesSync @{G ptr} = lines <$> (primIO $ prim__listBranchesSync ptr)

%foreign git_ffi "checkout_branch"
prim__checkoutBranch : Ptr GitRef
                    -> (branch : String)
                    -> (isNewBranch : Bool)
                    -> (onSuccess : String -> PrimIO ())
                    -> (onFailure : String -> PrimIO ())
                    -> PrimIO ()

public export
data BranchStatus = Existing | New

%runElab derive `{BranchStatus} [Eq]

export
checkoutBranch : Git => {default Existing b : BranchStatus} -> (branch : String) -> Promise' ()
checkoutBranch @{G ptr} {b=branchStatus} branch = 
  let isNewBranch = branchStatus == New
  in ignore . promiseIO $ prim__checkoutBranch ptr branch isNewBranch

%foreign git_ffi "push_new_branch"
prim__pushNewBranch : Ptr GitRef
                   -> (remoteName : String)
                   -> (branch : String)
                   -> (onSuccess : String -> PrimIO ())
                   -> (onFailure : String -> PrimIO ())
                   -> PrimIO ()

export
pushNewBranch : Git => (remoteName : String) -> (branch : String) -> Promise' ()
pushNewBranch @{G ptr} remoteName branch =
  ignore . promiseIO $ prim__pushNewBranch ptr remoteName branch

%foreign git_ffi "push"
prim__push : Ptr GitRef
          -> (onSuccess : String -> PrimIO ())
          -> (onFailure : String -> PrimIO ())
          -> PrimIO ()

export
push : Git => Promise' ()
push @{G ptr} = ignore . promiseIO $ prim__push ptr

%foreign git_ffi "list_remotes"
prim__listRemotes : Ptr GitRef
                 -> (onSuccess : String -> PrimIO ())
                 -> (onFailure : String -> PrimIO ())
                 -> PrimIO ()

export
listRemotes : Git => Promise' (List String)
listRemotes @{G ptr} = lines <$> (promiseIO $ prim__listRemotes ptr)

%foreign git_ffi "remote_uri"
prim__remoteURI : Ptr GitRef
               -> (remoteName : String)
               -> (onSuccess : String -> PrimIO ())
               -> (onFailure : String -> PrimIO ())
               -> PrimIO ()

||| Get the Git remote URI for the remote with the given name.
||| For example,
|||   "git@github.com:org/reponame.git"
|||   "https://github.com/org/reponame.git"
export
remoteURI : Git => (remoteName : String) -> Promise' String
remoteURI @{G ptr} remoteName = promiseIO $ prim__remoteURI ptr remoteName

%foreign git_ffi "remote_tracking_branch"
prim__remoteTrackingBranch : Ptr GitRef
                          -> (onSuccess : String -> PrimIO ())
                          -> (onFailure : String -> PrimIO ())
                          -> PrimIO ()

export
remoteTrackingBranch : Git => Promise' (Maybe String)
remoteTrackingBranch @{G ptr} = do
  str <- promiseIO $ prim__remoteTrackingBranch ptr
  pure $
    case strM str of
         StrNil         => Nothing
         (StrCons x xs) => Just str

%foreign git_ffi "uncommitted_changes"
prim__uncommittedChanges : Ptr GitRef
                        -> (onSuccess : String -> PrimIO ())
                        -> (onFailure : String -> PrimIO ())
                        -> PrimIO ()

||| Get the Git output for filenames with uncommitted changes. If there
||| are no files with uncommitted changes, returns @Nothing@.
export
uncommittedChanges : Git => Promise' (Maybe String)
uncommittedChanges @{G ptr} = do
  str <- promiseIO $ prim__uncommittedChanges ptr
  pure $
    case strM str of
         StrNil         => Nothing
         (StrCons x xs) => Just str

%foreign git_ffi "staged_changes"
prim__stagedChanges : Ptr GitRef
                   -> (onSuccess : String -> PrimIO ())
                   -> (onFailure : String -> PrimIO ())
                   -> PrimIO ()

||| Get the Git output for filenames with staged changes. If there
||| are no files with staged changes, returns @Nothing@.
export
stagedChanges : Git => Promise' (Maybe String)
stagedChanges @{G ptr} = do
  str <- promiseIO $ prim__stagedChanges ptr
  pure $
    case strM str of
         StrNil         => Nothing
         (StrCons x xs) => Just str

%foreign git_ffi "unpushed_commits"
prim__unpushedCommits : Ptr GitRef
                     -> (onSuccess : String -> PrimIO ())
                     -> (onFailure : String -> PrimIO ())
                     -> PrimIO ()

||| Get the Git output for unpushed commits (multiple lines per
||| commit including the ref, author, and commit message). If there
||| are no unpushed commits, returns @Nothing@.
export
unpushedCommits : Git => Promise' (Maybe String)
unpushedCommits @{G ptr} = do
  str <- promiseIO $ prim__unpushedCommits ptr
  pure $
    case strM str of
         StrNil         => Nothing
         (StrCons x xs) => Just str

%foreign git_ffi "user_email"
prim__userEmail : Ptr GitRef
               -> (onSuccess : String -> PrimIO ())
               -> (onFailure : String -> PrimIO ())
               -> PrimIO ()

export
userEmail : Git => Promise' String
userEmail @{G ptr} = promiseIO $ prim__userEmail ptr

%foreign git_ffi "root_dir"
prim__rootDir : Ptr GitRef
             -> (onSuccess : String -> PrimIO ())
             -> (onFailure : String -> PrimIO ())
             -> PrimIO ()

||| Get the absolute path of the Git repository's root directory
||| (the location of the `.git` folder).
export
rootDir : Git => Promise' String
rootDir @{G ptr} = promiseIO $ prim__rootDir ptr

