module System.Git

import Util.System
import Data.Promise

%default total

git : List String -> IO (String, String, Int)
git args = assert_total $ run' ("git" :: args)

promise : IO (String, String, Int) -> Promise' String
promise gitOp = liftIO res >>= either
  where
    res : IO (Either String String)
    res =  do
      (stdout, stderr, exitCode) <- gitOp
      case exitCode of
           0 => pure $ Right stdout
           _ => pure $ Left stderr

export
currentBranch : Promise' String
currentBranch = promise $ git ["branch", "--show-current"]

