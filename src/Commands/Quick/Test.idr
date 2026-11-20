module Commands.Quick.Test

import Commands.Quick

import Data.String

namespace Dasherize
  replacesSpacesWithDashes : branchSafeChars "a b c" === "a-b-c"
  replacesSpacesWithDashes = Refl

  replacesOthersWithDashes : branchSafeChars "JIRA-123_good[Test](of `chars`)" === "JIRA-123_good-Test-of-chars"
  replacesOthersWithDashes = Refl

namespace BranchNameSuggestion
  testUppercaseAndSpace : branchNameSuggestion "A branch-Name-candidate" === "a-branch-name-candidate"
  testUppercaseAndSpace = Refl
