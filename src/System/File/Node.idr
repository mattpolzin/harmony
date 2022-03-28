module System.File.Node

%foreign "node:lambda:(f) => require('fs').unlinkSync(f)"
prim__removeFile : String -> PrimIO ()

export
removeFile : HasIO io => (fname : String) -> io ()
removeFile = primIO . prim__removeFile

