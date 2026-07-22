module ShellCompletion.Common.Test

import ShellCompletion.Common
import Util.ShellCompletion
import Data.CompletionStyle

import Data.String
import Data.Issue
import Data.Date
import Data.List

-- testing
import Hedgehog
import TTest
--

-- Generators
%hint
unicodeGen : Gen String
unicodeGen = string (linear 0 30) unicode
--

namespace HashifyIfPrefix
  unhashedMatch : hashifyIfPrefix "12" 1234 === Just "#1234"
  unhashedMatch =
    let postulateIntegerShown : show 1234 === "1234"
        postulateIntegerShown = believe_me (Refl {x="1234"})
    in rewrite postulateIntegerShown in Refl

  unhashedNonMatch : hashifyIfPrefix "34" 1234 === Nothing
  unhashedNonMatch =
    let postulateIntegerShown : show 1234 === "1234"
        postulateIntegerShown = believe_me (Refl {x="1234"})
    in  rewrite postulateIntegerShown in Refl

namespace CompareAssignees
  noKnownGithubUser : (assignee1 : Maybe String) 
                   -> (assignee2 : Maybe String)
                   -> compareAssignees Nothing assignee1 assignee2 === EQ
  noKnownGithubUser _ _ = Refl

  unassignedsAreEq : (githubUser : String) 
                  -> compareAssignees (Just githubUser) Nothing Nothing === EQ
  unassignedsAreEq _ = Refl

  knownUserGreaterThanUnassigned1 : (githubUser : String) 
                                 -> compareAssignees (Just githubUser) Nothing (Just githubUser) ==> GT
  knownUserGreaterThanUnassigned1 g = MkTTest

  knownUserGreaterThanUnassigned2 : (githubUser : String)
                                 -> compareAssignees (Just githubUser) (Just githubUser) Nothing ==> LT
  knownUserGreaterThanUnassigned2 g = MkTTest

namespace IssuesByNumAndTitle
  testIssues : List Issue
  testIssues = [ MkIssue "g1" 1 "Issue 1" "A great issue" (MkDate 2026 01 01) "matt" Nothing Nothing
               , MkIssue "g2" 12 "Issue2" "Another great issue" (MkDate 2026 01 01) "matt" Nothing Nothing
               ]

  noIssues : issuesByNumAndTitle "" [] === []
  noIssues = Refl

  allIssuesForEmptyPrefix : 
    map Builtin.fst (issuesByNumAndTitle "" IssuesByNumAndTitle.testIssues)
      ==> ["#1", "Issue◌1", "#12", "Issue2"]
  allIssuesForEmptyPrefix = MkTTest

  allIssuesForNumberPrefix : 
    map Builtin.fst (issuesByNumAndTitle "1" IssuesByNumAndTitle.testIssues)
      ==> ["#1", "#12"]
  allIssuesForNumberPrefix = MkTTest

  allIssuesForTitlePrefix : 
    map Builtin.fst (issuesByNumAndTitle "I" IssuesByNumAndTitle.testIssues)
      ==> ["Issue◌1", "Issue2"]
  allIssuesForTitlePrefix = MkTTest

  allIssuesForUnslugifiedTitleMatch : 
    map Builtin.fst (issuesByNumAndTitle "Issue 1" IssuesByNumAndTitle.testIssues)
      ==> ["Issue◌1"]
  allIssuesForUnslugifiedTitleMatch = MkTTest

  sluggyPrefixMatchesTitle :
    map Builtin.fst (issuesByNumAndTitle "Issue◌" IssuesByNumAndTitle.testIssues)
      ==> ["Issue◌1"]
  sluggyPrefixMatchesTitle = MkTTest
