module Commands.Test

import Commands
import Util
import Util.String
import Util.OptionParsing

import Data.DPair
import Data.Either
import Data.List
import Data.List1
import Data.String
import Data.String.Extra

namespace ParsePrArgs
  parsesJustDraftFlag : parsePrArgs ["--draft"] === Right [Draft]
  parsesJustDraftFlag = Refl

  parsesJustReadyFlag : parsePrArgs ["--ready"] === Right [Ready]
  parsesJustReadyFlag = Refl

  parsesJustIssueFlag : parsePrArgs ["--issue"] === Right [CreateIssue]
  parsesJustIssueFlag = Refl

  parsesJustLabels : parsePrArgs ["#one", "#two"] === Right [Label "one", Label "two"]
  parsesJustLabels = Refl

  parsesIssueFlagWithDraftAndLabel : parsePrArgs ["--issue", "--draft", "#one"] === Right [CreateIssue, Draft, Label "one"]
  parsesIssueFlagWithDraftAndLabel = Refl

  parsesIntoOption : parsePrArgs ["--into", "a"] === Right [Into (Branch (Evidence "a" (IsNonEmpty "a")))]
  parsesIntoOption = Refl

  parsesProjectOption : parsePrArgs ["--project", "a"] === Right [SetProject (Title "a")]
  parsesProjectOption = Refl

  parsesOutputOption : parsePrArgs ["--output", "markdown"] === Right [Output Markdown]
  parsesOutputOption = Refl

  parsesOutputOptionWithPrintTree : parsePrArgs ["--print-tree", "--output", "markdown"] === Right [Output Markdown, PrintTree]
  parsesOutputOptionWithPrintTree = Refl

  errorsForJustIntoFlag : parsePrArgs ["--into"] === Left Commands.prUsageError
  errorsForJustIntoFlag = Refl

  errorsForJustOutputFlag : parsePrArgs ["--output"] === Left Commands.prUsageError
  errorsForJustOutputFlag = Refl

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

namespace ParseContributeArgs
  testList : parseContributeArgs ["--list"] === Right [List]
  testList = Refl

  testCheckout : parseContributeArgs ["--checkout"] === Right [Checkout]
  testCheckout = Refl

  testSkip : parseContributeArgs ["-3"] === Right [Skip 3]
  testSkip = Refl

  testSkipAndCheckout : parseContributeArgs ["--checkout", "-3"] === Right [Checkout, Skip 3]
  testSkipAndCheckout = Refl

  testIgnoreNum : parseContributeArgs ["--ignore", "234"] === Right [Ignore (PRNum 234)]
  testIgnoreNum = Refl

  testIgnoreUri : parseContributeArgs ["--ignore", "https://github.com/mattpolzin/harmony/pull/234"] === Right [Ignore (PRNum 234)]
  testIgnoreUri = Refl

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

