module Util.Debug

import Data.Promise

import System.File

export
writeDebugFile : HasIO io => String -> io ()
writeDebugFile = ignore . write fname
  where
    fname : String
    fname = "./harmony-debug.out"

    write : String -> String -> io ?
    write file contents = withFile file Append pure $
                            flip fPutStrLn contents
