module Graph

import Data.List
import Data.ReviewScore
import Data.SortedMap

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Symbols

%default total

data AugmentedReviewScore : login -> Type where
  Augmented : ReviewScore login -> (bonus : Nat) -> AugmentedReviewScore login

||| Graph a single line (bar) of dots.
||| @ indentation a number of leading spaces to product off to the left (uses Doc's @indent@)
||| @ score the net score to graph out in yellow.
||| @ detractor the amount detracting from the score, graphed in red.
||| @ bonus a bonus indicator graphed on the far right in green.
bar : (indentation : Nat) -> (score : Nat) -> (detractor : Nat) -> (bonus : Nat) -> Doc AnsiStyle
bar idt score detractor bonus = indent (cast idt) . hcat $
                              [ annotate (color Red) . hcat $ replicate detractor (pretty '◦')
                              , annotate (color Yellow) . hcat $ replicate score (pretty '·')
                              , annotate (color Green) . hcat $ replicate bonus (pretty '▪')
                              ]

graphOne : Pretty login => (highScore : Nat) -> (AugmentedReviewScore login) -> Doc AnsiStyle
graphOne highScore (Augmented (MkScore user partialScore combinedScore) bonus) =
  let idt    = highScore `minus` (partialScore + bonus)
      user   = annotate italic $ pretty user
      detractor = (partialScore `minus` combinedScore)
      remainingSpace = highScore `minus` combinedScore
      -- we create a bar with the combinedScore and then fill in any
      -- remaining space with an indication of the detractor. We cap
      -- the detractor representation at the high score to make everything
      -- line up nicely. The detractor is just there to give some indication
      -- of review requests that did not count positively toward the score.
  in  bar idt combinedScore (min remainingSpace detractor) bonus <++> user

graph : Pretty login => (highScore : Nat) -> List (AugmentedReviewScore login) -> Doc AnsiStyle
graph highScore = vsep . map (graphOne highScore)

||| Produce a graph of relative review workload for all developers matching the given
||| filter.
||| @ closedReviews    The logins of each reviewer of each closed PR (duplicates intact).
||| @ openReviews      The logins of each reviewer of each open PR (duplicates intact).
||| @ candidates       The logins of all potential reviewers that should be considered.
||| @ completedReviews Optionally pass a map from login to count of completed reviews to
|||                    graph as well.
export
reviewsGraph : Ord login => Pretty login =>
               (closedReviews : List login)
            -> (openReviews : List login)
            -> (candidates : List login)
            -> (completedReviews : Maybe (SortedMap login Nat))
            -> Doc AnsiStyle
reviewsGraph closedReviews openReviews candidates completedReviews =
  let scoredOptions = reverse $ scoredReviewers closedReviews openReviews (sort $ nub candidates)
      augmentedOptions : List (AugmentedReviewScore login) = 
        case completedReviews of
             Nothing        => (flip Augmented 0) <$> scoredOptions
             Just completed => augment completed <$> scoredOptions
      maxBonus = maybe 0 id (maxValue . SortedMap.toList <$> completedReviews)
  in  case scoredOptions of
           [] => emptyDoc
           ((MkScore _ s c) :: _) =>
             let highScore = c + (s `minus` c) + maxBonus
             in  header <+> graph (if highScore > 0 then highScore else 1) augmentedOptions <+> line
  where
    yellowDot : Doc AnsiStyle
    yellowDot = annotate (color Yellow) "·"

    redDot : Doc AnsiStyle
    redDot = annotate (color Red) "◦"

    greenBox : Doc AnsiStyle
    greenBox = annotate (color Green) "▪"

    header : Doc AnsiStyle
    header = vsep $
               catMaybes [ Just $ emptyDoc
                         , Just $ pretty "Weighted review workload."
                         , Just $ pretty "4x the number of open review requests" <++> parens yellowDot
                         , Just $ pretty "1x the number of closed PRs with unanswered review requests" <++> parens redDot
                         , if (null completedReviews) then Nothing else Just $ pretty "1x the number of completed reviews" <++> parens greenBox
                         , Just $ parens $ redDot <++> pretty "overlayed on" <++> yellowDot
                         , Just $ emptyDoc
                         , Just $ emptyDoc
                         ]

    augment : (completedReviews : SortedMap login Nat) -> ReviewScore login -> AugmentedReviewScore login
    augment completed score =
      Augmented score (maybe 0 id $ lookup score.user completed)

    maxValue : List (a, Nat) -> Nat
    maxValue [] = 0
    maxValue ((x, y) :: xs) = max y (maxValue xs)

