module Graph

import Data.List
import Data.ReviewScore

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Symbols

%default total

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

    ||| Graph a single line (bar) of dots.
    ||| @ indentation a number of leading spaces to product off to the left (uses Doc's @indent@)
    ||| @ score the net score to graph out in yellow.
    ||| @ detractor the amount detracting from the score, graphed in red.
    ||| @ bonus a bonus indicator graphed on the far right in green.
    bar : (indentation : Nat) -> (score : Nat) -> (detractor : Nat) -> (bonus : Nat) -> Doc AnsiStyle
    bar idt score detractor bonus = indent (cast idt) . hcat $
                                  [ annotate (color Red) . pretty $ replicate detractor '◦'
                                  , annotate (color Yellow) . pretty $ replicate score '·'
                                  , annotate (color Green) . pretty $ replicate bonus '▪'
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
      in  bar idt combinedScore (min remainingSpace detractor) 0 <++> user

    graph : (highScore : Nat) -> List (ReviewScore login) -> Doc AnsiStyle
    graph highScore = vsep . map (graphOne highScore)

