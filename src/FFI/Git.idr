module FFI.Git

import Data.Promise
import FFI

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
prim__currentBranch : Ptr GitRef -> (onSuccess : String -> PrimIO ()) -> (onFailure : String -> PrimIO ()) -> PrimIO ()

export
currentBranch : Git => Promise String
currentBranch @{(G ptr)} = promiseIO $ prim__currentBranch ptr

