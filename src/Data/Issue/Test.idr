module Data.Issue.Test

import Data.GHIssue
import Data.Date

import TTest

mockIssue : GHIssue
mockIssue = MkIssue
  { graphQlId = "1234"
  , number        = 12
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

  mockIssue2 : GHIssue
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

  mockIssue3 : GHIssue
  mockIssue3 = { body := bodyWithCloseWithPRComment } mockIssue

  testCloseWithPRParsing2 : (commentConfig BaseBranchGuess.mockIssue3).closeWithPr ==> Just True
  testCloseWithPRParsing2 = MkTTest

