module Util.Github.Test

import Util.Github

import TTest

namespace ParseGithubIssueNumber

  testIssueNumber  : 
        [ -- shrink from front
          parseGithubIssueNumber "feature/1234/hello"
        , parseGithubIssueNumber "/1234/hello"
        , parseGithubIssueNumber "1234/hello"
          -- shrink from back
        , parseGithubIssueNumber "feature/1234/"
        , parseGithubIssueNumber "feature/1234"
          -- just number
        , parseGithubIssueNumber "/1234/"
        , parseGithubIssueNumber "1234/"
        , parseGithubIssueNumber "/1234"
        , parseGithubIssueNumber "1234"
        ]
    ==> [ Just "1234"
        , Just "1234"
        , Just "1234"
        --
        , Just "1234"
        , Just "1234"
        --
        , Just "1234"
        , Just "1234"
        , Just "1234"
        , Just "1234"
        ]
  testIssueNumber = MkTTest

  testPrefixedIssueNumber :
        [ -- shrink from front
          parseGithubIssueNumber "feature/GH-1234/hello"
        , parseGithubIssueNumber "/GH-1234/hello"
        , parseGithubIssueNumber "GH-1234/hello"
          -- shrink from back
        , parseGithubIssueNumber "feature/GH-1234/"
        , parseGithubIssueNumber "feature/GH-1234"
          -- just number
        , parseGithubIssueNumber "/GH-1234/"
        , parseGithubIssueNumber "GH-1234/"
        , parseGithubIssueNumber "/GH-1234"
        , parseGithubIssueNumber "GH-1234"
        ]
    ==> [ Just "1234"
        , Just "1234"
        , Just "1234"
        --
        , Just "1234"
        , Just "1234"
        --
        , Just "1234"
        , Just "1234"
        , Just "1234"
        , Just "1234"
        ]
  testPrefixedIssueNumber = MkTTest


  testWithoutIssueNumber :
        [ parseGithubIssueNumber "Jira-1234/hello"
        , parseGithubIssueNumber "feature1234/hello"
        , parseGithubIssueNumber "feature/GHI1234/hello"
        , parseGithubIssueNumber "featureGH-1234/hello"
        ]
    ==> [ Nothing
        , Nothing
        , Nothing
        , Nothing
        ]
  testWithoutIssueNumber = MkTTest
