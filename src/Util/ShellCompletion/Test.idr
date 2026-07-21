module Util.ShellCompletion.Test

import Util.ShellCompletion

import Data.CompletionStyle
import Data.String

namespace Unhashify
  test1 : unhashify "" = ""
  test1 = Refl

  test2 : unhashify "\\" = "\\"
  test2 = Refl

  test3 : unhashify "#hello" = "hello"
  test3 = Refl

  test4 : unhashify "\\hello" = "\\hello"
  test4 = Refl

  test5 : unhashify "\\#hello" = "hello"
  test5 = Refl

namespace SomeFrom
  matchesAllOpts1 : someFrom @{Cmd} ["--bugfix"] ["--bugfix"] === Just ["--bugfix"]
  matchesAllOpts1 = Refl

  matchesAllOpts2 : someFrom @{Cmd} ["--one", "--two"] ["--one", "--two"] === Just ["--one", "--two"]
  matchesAllOpts2 = Refl

  matchesOneOpt : someFrom @{Cmd} ["--bugfix"] ["--one", "--two", "--bugfix", "--three"] === Just ["--bugfix"]
  matchesOneOpt = Refl

  matchesWithDescription : someFrom @{CmdAndDescription} ["--bugfix"] [("--one", "one"), ("--bugfix", "hello"), ("--three", "three")] === Just ["--bugfix:hello"]
  matchesWithDescription = Refl

