module Commands.PullRequest.Test

import Data.Config
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

namespace RemoveCommentTags
  withoutTags : removeCommentTags "Hello.\nHi Hello\n" ==> "Hello.\nHi Hello\n"
  withoutTags = MkTTest

  withTags : removeCommentTags "<!-- Hello.\nHi Hello\n-->\n" ==> " Hello.\nHi Hello\n"
  withTags = MkTTest

namespace RenderPrTree
  config : Config
  config =
    { addPrTreeDescription := True } simpleDefaults

  noPrs : renderPrTree @{RenderPrTree.config} Markdown (prTree (Just "feature-1") Nothing [] [] "main")
            ==> """
                > ⨀ `main`
                >> ↖ `feature-1`
                
                """
  noPrs = MkTTest

  noPrsShell : renderPrTree @{RenderPrTree.config} Shell (prTree (Just "feature-1") Nothing [] [] "main")
            ==> """
                 ⨀ main
                     ↖ feature-1
                
                """
  noPrsShell = MkTTest

  mkPr : Integer -> String -> PullRequest
  mkPr num headRef = MkPullRequest num "Fancy PR" (MkDate 2025 01 01) False "" Open [] headRef ""

  onePr : renderPrTree @{RenderPrTree.config} Markdown (prTree (Just "feature-2") Nothing [] [mkPr 123 "feature-1"] "main")
            ==> """
                > ⨀ `main`
                >> ↖ `feature-1` (https://github.com/org/repo/pull/123)
                >> **Fancy PR**
                >>> ↖ `feature-2`

                """
  onePr = MkTTest

  onePrShell : renderPrTree @{RenderPrTree.config} Shell (prTree (Just "feature-2") Nothing [] [mkPr 123 "feature-1"] "main")
            ==> """
                 ⨀ main
                     ↖ Fancy PR
                       ├ feature-1
                       └ https://github.com/org/repo/pull/123
                         ↖ feature-2

                """
  onePrShell = MkTTest

  onePrPlusTitle : renderPrTree @{RenderPrTree.config} Markdown (prTree (Just "feature-2") (Just "Title") [] [mkPr 123 "feature-1"] "main")
            ==> """
                > ⨀ `main`
                >> ↖ `feature-1` (https://github.com/org/repo/pull/123)
                >> **Fancy PR**
                >>> ↖ `feature-2`
                >>> **Title**

                """
  onePrPlusTitle = MkTTest

  onePrPlusTitleShell : renderPrTree @{RenderPrTree.config} Shell (prTree (Just "feature-2") (Just "Title") [] [mkPr 123 "feature-1"] "main")
            ==> """
                 ⨀ main
                     ↖ Fancy PR
                       ├ feature-1
                       └ https://github.com/org/repo/pull/123
                         ↖ Title

                """
  onePrPlusTitleShell = MkTTest

  onePrNoLeaf : renderPrTree @{RenderPrTree.config} Markdown (prTree Nothing Nothing [] [mkPr 123 "feature-1"] "main")
                  ==> """
                      > ⨀ `main`
                      >> ↖ `feature-1` (https://github.com/org/repo/pull/123)
                      >> **[[** _you are here_ **]]**
                      >> **Fancy PR**

                      """
  onePrNoLeaf = MkTTest

  onePrNoLeafShell : renderPrTree @{RenderPrTree.config} Shell (prTree Nothing Nothing [] [mkPr 123 "feature-1"] "main")
                  ==> """
                       ⨀ main
                           ↖ ▪ Fancy PR
                             ├ feature-1
                             └ https://github.com/org/repo/pull/123

                      """
  onePrNoLeafShell = MkTTest

  onePrAboveOneBelowNoLeaf : renderPrTree @{RenderPrTree.config} Markdown (prTree Nothing Nothing [mkPr 456 "feature-2"] [mkPr 123 "feature-1"] "main")
                  ==> """
                      > ⨀ `main`
                      >> ↖ `feature-1` (https://github.com/org/repo/pull/123)
                      >> **[[** _you are here_ **]]**
                      >> **Fancy PR**
                      >>> ↖ `feature-2` (https://github.com/org/repo/pull/456)
                      >>> **Fancy PR**

                      """
  onePrAboveOneBelowNoLeaf = MkTTest

  onePrAboveOneBelowNoLeafShell : renderPrTree @{RenderPrTree.config} Shell (prTree Nothing Nothing [mkPr 456 "feature-2"] [mkPr 123 "feature-1"] "main")
                  ==> """
                       ⨀ main
                           ↖ ▪ Fancy PR
                             ├ feature-1
                             └ https://github.com/org/repo/pull/123
                               ↖ Fancy PR
                                 ├ feature-2
                                 └ https://github.com/org/repo/pull/456

                      """
  onePrAboveOneBelowNoLeafShell = MkTTest

