module Commands.Quick.Test

import Commands.Quick

import Data.String

namespace Dasherize
  replacesSpacesWithDashes : dasherize "a b c" === "a-b-c"
  replacesSpacesWithDashes = Refl

namespace BranchNameSuggestion
  testUppercaseAndSpace : branchNameSuggestion "A branch-Name-candidate" === "a-branch-name-candidate"
  testUppercaseAndSpace = Refl
