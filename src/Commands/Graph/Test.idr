module Commands.Graph.Test

import Commands.Graph

import Data.Config
import Data.SortedMap
import Data.Theme
import Util

import TTest

config : Config
config =
    { teamSlugs  := ["team1", "team2"]
    , repoLabels := ["label1", "label2", "label3"]
    , orgMembers := ["sam", "gretta", "florence"]
    } simpleDefaults

namespace ReviewsGraph

  renderTestReviewsGraph : ({x : Type} -> x -> Maybe x) -> String
  renderTestReviewsGraph f =
    let cfg = config
        graph = Commands.Graph.reviewsGraph {login=String} 
                  ["sam", "gretta", "gretta", "gretta", "florence", "florence"]
                  ["sam", "sam", "gretta"]
                  ["sam", "gretta", "florence"]
                  (f $ fromList [("sam", 3), ("florence", 2)])
    in  Util.renderString graph

  testCompleted : renderTestReviewsGraph Just
    ==> """

        Weighted review workload.
        4x the number of open review requests (·)
        1x the number of closed PRs with unanswered review requests (◦)
        1x the number of completed reviews (▪)
        (◦ overlayed on ·)

        ◦······· sam      ▪▪▪
            ◦◦◦· gretta   
              ◦◦ florence ▪▪

        Note: It is a strongly held opinion of Harmony that the above graph is not a
        measure of each developers' productivity. It is a proxy for each developers'
        existing PR review workload that may help to understand how Harmony is choosing
        reviewers or similarly help one developer decide which other developers have
        capacity to help review their work.

        """
  testCompleted = MkTTest

  testWithoutCompleted : renderTestReviewsGraph (const Nothing)
    ==> """

        Weighted review workload.
        4x the number of open review requests (·)
        1x the number of closed PRs with unanswered review requests (◦)
        (◦ overlayed on ·)

        ◦······· sam      
            ◦◦◦· gretta   
              ◦◦ florence 

        Note: It is a strongly held opinion of Harmony that the above graph is not a
        measure of each developers' productivity. It is a proxy for each developers'
        existing PR review workload that may help to understand how Harmony is choosing
        reviewers or similarly help one developer decide which other developers have
        capacity to help review their work.

        """
  testWithoutCompleted = MkTTest

