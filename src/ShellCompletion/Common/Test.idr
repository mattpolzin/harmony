module ShellCompletion.Common.Test

import ShellCompletion.Common
import Util.ShellCompletion
import Data.CompletionStyle

import Data.String

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
  testUnhashedMatch : hashifyIfPrefix "12" 1234 === Just "#1234"
  testUnhashedMatch =
    let postulateIntegerShown : show 1234 === "1234"
        postulateIntegerShown = believe_me (Refl {x="1234"})
    in rewrite postulateIntegerShown in Refl

  testUnhashedNonMatch : hashifyIfPrefix "34" 1234 === Nothing
  testUnhashedNonMatch =
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
