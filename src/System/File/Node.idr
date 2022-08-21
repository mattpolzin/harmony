module System.File.Node

%default total

-- TODO: Remove this once Idris2 gains a version newer than 0.5.1. This has been added to the base library.

%foreign "node:lambda:(f) => require('fs').unlinkSync(f)"
prim__removeFile : String -> PrimIO ()

export
removeFile : HasIO io => (fname : String) -> io ()
removeFile = primIO . prim__removeFile

