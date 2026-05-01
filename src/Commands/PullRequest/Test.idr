module Commands.PullRequest.Test

import Data.PullRequest
import Commands.PullRequest
import TTest

namespace PrCreationUrl
  withoutIntoBranch : prCreationUrl "org" "repo" "branch" Nothing === "https://github.com/org/repo/compare/branch?expand=1"
  withoutIntoBranch = Refl

  withIntoBranch : prCreationUrl "org" "repo" "branch" (Just "main") === "https://github.com/org/repo/compare/main...branch?expand=1"
  withIntoBranch = Refl

namespace RenderPrTree
  noPrs : renderPrTree "feature-1" [] "main" ==> """
                                                 `main`
                                                   ↖ `feature-1`
                                                 """
  noPrs = MkTTest

