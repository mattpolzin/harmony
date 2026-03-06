module Commands.Test

import Commands
import Util

import Data.DPair
import Data.Either
import Data.List
import Data.String
import Data.String.Extra

namespace ParsePrArgs
  parsesJustDraftFlag : parsePrArgs ["--draft"] === Right [Draft]
  parsesJustDraftFlag = Refl

  parsesJustReadyFlag : parsePrArgs ["--ready"] === Right [Ready]
  parsesJustReadyFlag = Refl

  parsesJustLabels : parsePrArgs ["#one", "#two"] === Right [Label "one", Label "two"]
  parsesJustLabels = Refl

  parsesIntoOption : parsePrArgs ["--into", "a"] === Right [Into (Branch (Evidence "a" (IsNonEmpty "a")))]
  parsesIntoOption = Refl

  errorsForJustIntoFlag : parsePrArgs ["--into"] === Left Commands.prUsageError
  errorsForJustIntoFlag = Refl

  errorsForLabelWithoutHash : parsePrArgs ["a"] === Left Commands.prUsageError
  errorsForLabelWithoutHash = Refl

namespace ParseGraphArgs
  testJustTeamName : parseGraphArgs ["team1"] === Right [TeamName "team1"]
  testJustTeamName = Refl

  testRequiresOneArgument : parseGraphArgs [] === Left "The graph command expects the name of a GitHub Team and optionally --completed as arguments."
  testRequiresOneArgument = Refl

  testAcceptsAtMostTwoArguments : parseGraphArgs ["a", "b", "c"] === Left "graph accepts at most one team name and the --completed flag."
  testAcceptsAtMostTwoArguments = Refl

  testParsesCompleted : parseGraphArgs ["--completed"] === Right [IncludeCompletedReviews]
  testParsesCompleted = Refl

  testParsesCompletedShorthand : parseGraphArgs ["-c"] === Right [IncludeCompletedReviews]
  testParsesCompletedShorthand = Refl

  testParsesTeamArgument : parseGraphArgs ["developers"] === Right [TeamName "developers"]
  testParsesTeamArgument = Refl

  testParsesTeamArgumentAndCompletedFlag : parseGraphArgs ["developers", "--completed"] === Right [TeamName "developers", IncludeCompletedReviews]
  testParsesTeamArgumentAndCompletedFlag = Refl

  testParsesCompletedFlagAndTeamArgument : parseGraphArgs ["--completed", "developers"] === Right [IncludeCompletedReviews, TeamName "developers"]
  testParsesCompletedFlagAndTeamArgument = Refl

namespace ParseQuickArgs
  testBugfix : parseQuickArgs ["--bugfix"] === [ABugfix]
  testBugfix = Refl

  testBugfixLast : parseQuickArgs ["a", "bug", "--bugfix"] === [IssueNumOrTitle "a", IssueNumOrTitle "bug", ABugfix]
  testBugfixLast = Refl

  testBugfixFirst : parseQuickArgs ["--bugfix", "a", "bug"] === [ABugfix, IssueNumOrTitle "a", IssueNumOrTitle "bug"]
  testBugfixFirst = Refl

  testBugfixMiddle : parseQuickArgs ["a", "--bugfix", "bug"] === [IssueNumOrTitle "a", ABugfix, IssueNumOrTitle "bug"]
  testBugfixMiddle = Refl

namespace TitleArg
  testConcatsTitleStrings : titleArg [IssueNumOrTitle "One", IssueNumOrTitle "Two"] === Just "One Two"
  testConcatsTitleStrings = Refl

  testSkipsBugfixArgs : titleArg [IssueNumOrTitle "One", ABugfix] === Just "One"
  testSkipsBugfixArgs = Refl

  testNothingForOnlyBugfix : titleArg [ABugfix] === Nothing
  testNothingForOnlyBugfix = Refl

namespace TitleOrNumberArg
  singleHashArgumentIsIssueNumber : titleOrNumberArg [IssueNumOrTitle "#1234"] === IssueNumber "1234"
  singleHashArgumentIsIssueNumber = Refl

  singleNonHashArgumentIsTitle : titleOrNumberArg [IssueNumOrTitle "1234"] === IssueTitle "1234"
  singleNonHashArgumentIsTitle = Refl

  multipleArgumentsIsTitle : titleOrNumberArg [IssueNumOrTitle "#1234", IssueNumOrTitle "hi"] === IssueTitle "#1234 hi"
  multipleArgumentsIsTitle = Refl

  singleHashArgPlusFlagIsNumber : titleOrNumberArg [ABugfix, IssueNumOrTitle "#1234"] === IssueNumber "1234"
  singleHashArgPlusFlagIsNumber = Refl

  singleHashArgPlusFlagIsNumber' : titleOrNumberArg [IssueNumOrTitle "#1234", ABugfix] === IssueNumber "1234"
  singleHashArgPlusFlagIsNumber' = Refl

namespace IssueCategory
  testPicksBugfixUpLast : issueCategory [IssueNumOrTitle "hello", ABugfix] === Bugfix
  testPicksBugfixUpLast = Refl

  testPicksBugfixUpFirst : issueCategory [ABugfix, IssueNumOrTitle "hello"] === Bugfix
  testPicksBugfixUpFirst = Refl

  testNotBugfix : issueCategory [IssueNumOrTitle "hi", IssueNumOrTitle "hello"] === Feature
  testNotBugfix = Refl

  testNotBugfixSimple : issueCategory [] === Feature
  testNotBugfixSimple = Refl

