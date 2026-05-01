module Commands.PullRequest.Test

import Data.Date
import Data.PullRequest
import Data.Theme

import Commands.PullRequest

import TTest

namespace PrCreationUrl
  withoutIntoBranch : prCreationUrl "org" "repo" "branch" Nothing === "https://github.com/org/repo/compare/branch?expand=1"
  withoutIntoBranch = Refl

  withIntoBranch : prCreationUrl "org" "repo" "branch" (Just "main") === "https://github.com/org/repo/compare/main...branch?expand=1"
  withIntoBranch = Refl

namespace RenderPrTree
  noPrs : renderPrTree Dark Markdown "org" "repo" (Just "feature-1") [] "main"
            ==> """
                ● `main`
                    ↖ `feature-1`
                """
  noPrs = MkTTest

  mkPr : Integer -> String -> PullRequest
  mkPr num headRef = MkPullRequest num "" (MkDate 2025 01 01) False "" Open [] headRef ""

  onePr : renderPrTree Dark Markdown "org" "repo"  (Just "feature-2") [mkPr 123 "feature-1"] "main"
            ==> """
                ● `main`
                    ↖ `feature-1` (#123)
                        ↖ `feature-2`
                """
  onePr = MkTTest

  onePrNoLeaf : renderPrTree Dark Markdown "org" "repo"  Nothing [mkPr 123 "feature-1"] "main"
                  ==> """
                      ● `main`
                          ↖ `feature-1` (#123)
                      
                      """
  onePrNoLeaf = MkTTest

