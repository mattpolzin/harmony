module FFI.Git

import Data.String
import Data.Promise
import FFI

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
currentBranch : Git => Promise String
currentBranch @{G ptr} = promiseIO $ prim__currentBranch ptr

%foreign git_ffi "checkout_branch"
prim__checkoutBranch : Ptr GitRef
                    -> (branch : String)
                    -> (onSuccess : String -> PrimIO ())
                    -> (onFailure : String -> PrimIO ())
                    -> PrimIO ()

export
checkoutBranch : Git => (branch : String) -> Promise ()
checkoutBranch @{G ptr} branch = 
  ignore . promiseIO $ prim__checkoutBranch ptr branch

%foreign git_ffi "push_new_branch"
prim__pushNewBranch : Ptr GitRef
                   -> (remoteName : String)
                   -> (branch : String)
                   -> (onSuccess : String -> PrimIO ())
                   -> (onFailure : String -> PrimIO ())
                   -> PrimIO ()

export
pushNewBranch : Git => (remoteName : String) -> (branch : String) -> Promise ()
pushNewBranch @{G ptr} remoteName branch =
  ignore . promiseIO $ prim__pushNewBranch ptr remoteName branch

%foreign git_ffi "list_remotes"
prim__listRemotes : Ptr GitRef
                 -> (onSuccess : String -> PrimIO ())
                 -> (onFailure : String -> PrimIO ())
                 -> PrimIO ()

export
listRemotes : Git => Promise (List String)
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
remoteURI : Git => (remoteName : String) -> Promise String
remoteURI @{G ptr} remoteName = promiseIO $ prim__remoteURI ptr remoteName

%foreign git_ffi "remote_tracking_branch"
prim__remoteTrackingBranch : Ptr GitRef
                          -> (onSuccess : String -> PrimIO ())
                          -> (onFailure : String -> PrimIO ())
                          -> PrimIO ()

export
remoteTrackingBranch : Git => Promise (Maybe String)
remoteTrackingBranch @{G ptr} =
  do str <- promiseIO $ prim__remoteTrackingBranch ptr
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
userEmail : Git => Promise String
userEmail @{G ptr} = promiseIO $ prim__userEmail ptr

%foreign git_ffi "root_dir"
prim__rootDir : Ptr GitRef
             -> (onSuccess : String -> PrimIO ())
             -> (onFailure : String -> PrimIO ())
             -> PrimIO ()

||| Get the absolute path of the Git repository's root directory
||| (the location of the `.git` folder).
export
rootDir : Git => Promise String
rootDir @{G ptr} = promiseIO $ prim__rootDir ptr

