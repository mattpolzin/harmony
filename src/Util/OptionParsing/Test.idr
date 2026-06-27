module Util.OptionParsing.Test

import Data.String
import Util.OptionParsing

import TTest
import Hedgehog

namespace ParseAllOfOpt
  emptyArgsEmptyOpts : parseAllOfOpt ["-f", "--flag"] (\s => Just s) [] === ([], [])
  emptyArgsEmptyOpts = Refl

  nonFlagIsLeftOver : parseAllOfOpt ["-f", "--flag"] (\s => Just s) ["hello"] === ([], ["hello"])
  nonFlagIsLeftOver = Refl

  flagWithNoOptionIsLeftOver1 : parseAllOfOpt ["-f", "--flag"] (\s => Just s) ["--flag"] === ([], ["--flag"])
  flagWithNoOptionIsLeftOver1 = Refl

  flagWithNoOptionIsLeftOver2 : parseAllOfOpt ["-f", "--flag"] (\s => Just s) ["hello", "--flag"] === ([], ["hello", "--flag"])
  flagWithNoOptionIsLeftOver2 = Refl

  flagWithOptionTaken1 : parseAllOfOpt ["-f", "--flag"] (\s => Just s) ["--flag", "hello"] === (["hello"], [])
  flagWithOptionTaken1 = Refl

  flagWithOptionTaken2 : parseAllOfOpt ["-f", "--flag"] (\s => Just s) ["ok", "--flag", "hello", "hi"] === (["hello"], ["ok", "hi"])
  flagWithOptionTaken2 = Refl

  flagWithInvalidOptionLeftOver : parseAllOfOpt ["-f", "--flag"] (\s => Nothing) ["--flag", "hi"] === ([], ["--flag", "hi"])
  flagWithInvalidOptionLeftOver = Refl

  shortFlag : parseAllOfOpt ["-f", "--flag"] (\s => Just s) ["-f", "hi"] === (["hi"], [])
  shortFlag = Refl

  onlyLongFlag : parseAllOfOpt ["--flag"] (\s => Just s) ["--flag", "hi"] === (["hi"], [])
  onlyLongFlag = Refl

