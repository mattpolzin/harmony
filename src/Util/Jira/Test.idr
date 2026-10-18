module Util.Jira.Test

import Util.Jira

import TTest

namespace ParseJiraSlug
  testIssueSlug :
        [ -- shrink from front
          parseJiraSlug "feature/PROJ-1234/hello"
        , parseJiraSlug "/PROJ-1234/hello"
        , parseJiraSlug "PROJ-1234/hello"
          -- shrink from back
        , parseJiraSlug "feature/PROJ-1234/"
        , parseJiraSlug "feature/PROJ-1234"
          -- just number
        , parseJiraSlug "/PROJ-1234/"
        , parseJiraSlug "PROJ-1234/"
        , parseJiraSlug "/PROJ-1234"
        , parseJiraSlug "PROJ-1234"
          -- dasherized as prefix
        , parseJiraSlug "PROJ-1234-thing-one"
          -- lowercased
        , parseJiraSlug "proj-1234-thing-one"
        ]
    ==> [ Just "PROJ-1234"
        , Just "PROJ-1234"
        , Just "PROJ-1234"
        --
        , Just "PROJ-1234"
        , Just "PROJ-1234"
        --
        , Just "PROJ-1234"
        , Just "PROJ-1234"
        , Just "PROJ-1234"
        , Just "PROJ-1234"
        --
        , Just "PROJ-1234"
        --
        , Just "proj-1234"
        ]
  testIssueSlug = MkTTest


  testWithoutIssueSlug :
        [ parseJiraSlug "1234/hello"
        , parseJiraSlug "feature1234/hello"
        , parseJiraSlug "PROJ1234"
        ]
    ==> [ Nothing
        , Nothing
        , Nothing
        ]
  testWithoutIssueSlug = MkTTest
