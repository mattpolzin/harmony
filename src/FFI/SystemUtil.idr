module FFI.SystemUtil

import FFI

import public System.File.Mode
import System.File.Error
import System.File.Types

%default total

system_util_ffi : (fnName : String) -> String
system_util_ffi = node_ffi "system_util"

data FilePlus : Type where [external]

FilePtrPlus : Type
FilePtrPlus = Ptr FilePlus

export
data Process = PPtr FilePtrPlus

%foreign system_util_ffi "popen_plus"
prim__popen_plus : String -> String -> PrimIO FilePtrPlus

export
popen' : HasIO io => (command : String) -> (mode : Mode) -> io (Either FileError Process)
popen' cmd mode = do
    ptr <- primIO (prim__popen_plus cmd (modeStr mode))
    if prim__nullPtr ptr /= 0
        then returnError
        else pure (Right (PPtr ptr))

%foreign system_util_ffi "pclose_plus"
prim__pclose_plus : FilePtrPlus -> PrimIO Int

export
pclose' : HasIO io => Process -> io Int
pclose' (PPtr ptr) = primIO (prim__pclose_plus ptr)

%foreign system_util_ffi "get_pipe_fd"
prim__get_pipe_fd : FilePtrPlus -> FilePtr

%foreign system_util_ffi "get_err_fd"
prim__get_err_fd : FilePtrPlus -> FilePtr

export
getPipeFile : Process -> File
getPipeFile (PPtr ptr) = FHandle (prim__get_pipe_fd ptr)

export
getErrFile : Process -> File
getErrFile (PPtr ptr) = FHandle (prim__get_err_fd ptr)
