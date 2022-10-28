module Graph

import Data.List
import Data.ReviewScore

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Symbols

||| Produce a graph of relative review workload for all developers matching the given
||| filter.
||| @ closedReviews The logins of each reviewer of each closed PR (duplicates intact).
||| @ openReviews   The logins of each reviewer of each open PR (duplicates intact).
||| @ candidates    The logins of all potential reviewers that should be considered.
export
reviewsGraph : Ord login => Pretty login =>
               (closedReviews : List login)
            -> (openReviews : List login)
            -> (candidates : List login)
            -> Doc AnsiStyle
reviewsGraph closedReviews openReviews candidates =
  let scoredOptions = reverse $ scoredReviewers closedReviews openReviews (sort $ nub candidates)
  in  case scoredOptions of
           [] => emptyDoc
           ((MkScore _ s c) :: _) =>
             let highScore = c + (s `minus` c)
             in  header <+> graph (if highScore > 0 then highScore else 1) scoredOptions <+> line
  where
    yellowDot : Doc AnsiStyle
    yellowDot = annotate (color Yellow) "·"

    redDot : Doc AnsiStyle
    redDot = annotate (color Red) "◦"

    header : Doc AnsiStyle
    header = vsep [ emptyDoc
                  , pretty "Weighted review workload."
                  , pretty "4x the number of open review requests" <++> parens yellowDot
                  , pretty "1x the number of closed PRs with unanswered review requests" <++> parens redDot
                  , parens $ redDot <++> pretty "overlayed on" <++> yellowDot
                  , emptyDoc
                  , emptyDoc
                  ]

    -- The "detractor" is an indication of the amount of the score that was taken
    -- away by the heuristic in `scoredReviewers` that weights closed reviews with
    -- unanswered review requests negatively.
    bar : (indentation : Nat) -> (score : Nat) -> (detractor : Nat) -> Doc AnsiStyle
    bar idt score detractor = indent (cast idt) . hcat $
                                [ annotate (color Red) . pretty $ replicate detractor '◦'
                                , annotate (color Yellow) . pretty $ replicate score '·'
                                ]

    graphOne : (highScore : Nat) -> (ReviewScore login) -> Doc AnsiStyle
    graphOne highScore (MkScore user partialScore combinedScore) =
      let idt    = highScore `minus` partialScore
          user   = annotate italic $ pretty user
          detractor = (partialScore `minus` combinedScore)
          remainingSpace = highScore `minus` combinedScore
          -- we create a bar with the combinedScore and then fill in any
          -- remaining space with an indication of the detractor. We cap
          -- the detractor representation at the high score to make everything
          -- line up nicely. The detractor is just there to give some indication
          -- of review requests that did not count positively toward the score.
      in  bar idt combinedScore (min remainingSpace detractor) <++> user

    graph : (highScore : Nat) -> List (ReviewScore login) -> Doc AnsiStyle
    graph highScore = vsep . map (graphOne highScore)

