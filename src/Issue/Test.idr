module Issue.Test

import Data.String
import Issue

import TTest

namespace GetCLIIssuePrefix
  nothingForNoPrefix : getCLIIssuePrefix "hello" === Nothing
  nothingForNoPrefix = Refl

  nothingForPercentInMiddle : getCLIIssuePrefix "he%llo" === Nothing
  nothingForPercentInMiddle = Refl

  percentForPercentPrefix : getCLIIssuePrefix "%hello" === Just (Percent "hello")
  percentForPercentPrefix = Refl

  hashForHashtagPrefix : getCLIIssuePrefix "#hello" === Just (Hashtag "hello")
  hashForHashtagPrefix = Refl

  hashForEscapedHashtagPrefix : getCLIIssuePrefix "\#hello" === Just (Hashtag "hello")
  hashForEscapedHashtagPrefix = Refl

