module Util.System

import FFI.SystemUtil

import System.Escape
import System.File.ReadWrite

%default total

||| Run a shell command, returning its stdout, stderr, and exit code.
export
covering
run' : HasIO io => (cmd : String) -> io (String, String, Int)
run' cmd = do
    Right p <- popen' cmd Read
        | Left err => pure ("", "", 1)
    let stdout = getPipeFile p
    let stderr = getErrFile p
    Right output <- fRead stdout
        | Left err => pure ("", "", 1)
    Right err <- fRead stderr
        | Left err => pure ("", "", 1)
    exitCode <- pclose' p
    pure (output, err, exitCode)

namespace Escaped
  export
  covering
  run' : HasIO io => (cmd : List String) -> io (String, String, Int)
  run' = run' . escapeCmd

