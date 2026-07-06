module Data.Issue.Test

import Data.Issue
import Data.Date

import TTest

mockIssue : Issue
mockIssue = MkIssue
  { number        = 12
  , title         = "issue 1"
  , body          = "issue description"
  , createdAt     = MkDate 2020 10 01
  , author        = "mattpolzin"
  , assignee      = Nothing
  , linkedPRCount = Nothing
  }

namespace BaseBranchGuess
  testBaseBranchParsing : Test.mockIssue.commentConfig.baseBranchGuess ==> Nothing
  testBaseBranchParsing = MkTTest

  bodyWithBaseBranch : String
  bodyWithBaseBranch =
    """
    <!-- base-branch: feature/1234/cool-stuff
    -->
    """

  mockIssue2 : Issue
  mockIssue2 = { body := bodyWithBaseBranch } mockIssue

  testBaseBranchParsing2 : BaseBranchGuess.mockIssue2.commentConfig.baseBranchGuess ==> Just "feature/1234/cool-stuff"
  testBaseBranchParsing2 = MkTTest

  testCloseWithPRParsing : Test.mockIssue.commentConfig.closeWithPr ==> Nothing
  testCloseWithPRParsing = MkTTest

  bodyWithCloseWithPRComment : String
  bodyWithCloseWithPRComment =
    """
    <!-- close-with-pr: yes
    -->
    """

  mockIssue3 : Issue
  mockIssue3 = { body := bodyWithCloseWithPRComment } mockIssue

  testCloseWithPRParsing2 : BaseBranchGuess.mockIssue3.commentConfig.closeWithPr ==> Just True
  testCloseWithPRParsing2 = MkTTest

