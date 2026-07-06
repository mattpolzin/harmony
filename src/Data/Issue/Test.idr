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
  testBaseBranchParsing : (commentConfig Test.mockIssue).baseBranchGuess ==> Nothing
  testBaseBranchParsing = MkTTest

  bodyWithBaseBranch : String
  bodyWithBaseBranch =
    """
    <!-- base-branch: feature/1234/cool-stuff
    -->
    """

  mockIssue2 : Issue
  mockIssue2 = { body := bodyWithBaseBranch } mockIssue

  testBaseBranchParsing2 : (commentConfig BaseBranchGuess.mockIssue2).baseBranchGuess ==> Just "feature/1234/cool-stuff"
  testBaseBranchParsing2 = MkTTest

  testCloseWithPRParsing : (commentConfig Test.mockIssue).closeWithPr ==> Nothing
  testCloseWithPRParsing = MkTTest

  bodyWithCloseWithPRComment : String
  bodyWithCloseWithPRComment =
    """
    <!-- close-with-pr: yes
    -->
    """

  mockIssue3 : Issue
  mockIssue3 = { body := bodyWithCloseWithPRComment } mockIssue

  testCloseWithPRParsing2 : (commentConfig BaseBranchGuess.mockIssue3).closeWithPr ==> Just True
  testCloseWithPRParsing2 = MkTTest

