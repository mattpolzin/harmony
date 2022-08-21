module Reviewer

import Data.List
import Data.List.DeleteBy
import Data.List1
import Data.Nat
import Data.String
import System.Random.Node
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Symbols

%default total

record Score login where
  constructor MkScore
  user          : login
  partialScore  : Nat
  combinedScore : Nat

loginScore : Score login -> (login, Nat)
loginScore (MkScore l s c) = (l, c)

||| Get scored reviewers sorted from lowest to highest score.
||| A low score indicates less review work (a better candidate for
||| assigning a new PR to).
|||
||| The resulting `Score` contains both a partial score (the score
||| based only on open reivews) and also a combined score (the score
||| after negatively weighting closed reviews).
|||
||| Closed reviews are negatively scored because they are review requests
||| that were not answered.
scoredReviewers : Ord login =>
                  (closedReviews : List login)
               -> (openReviews : List login)
               -> (candidates : List login)
               -> List (Score login)
scoredReviewers closedReviews openReviews candidates =
  let closedReviewsWeighted = weightReviews 1 closedReviews
      openReviewsWeighted   = weightReviews 4 openReviews
      allReviews            = zipReviews openReviewsWeighted closedReviewsWeighted Subtract False
  in sort' $ zipReviews allReviews (weightReviews 0 candidates) Add True
  where
      -- The number of appearances is multiplied by the supplied weight which 
      -- allows us to weight some lists more heavily than others.
      weightReviews : (weight : Nat) -> List login -> List (Score login)
      weightReviews weight reviews =
        let grouped = groupAllWith id reviews
        in  grouped <&> (\xs => let score = (* weight) . length $ forget xs in (MkScore (head xs) score score))

      -- Sort logins by the number of times each login was requested for review.
      sort' : List (Score login) -> List (Score login)
      sort' = sortBy $ compare `on` combinedScore

      data Op = Add | Subtract

      -- Add or subtract the scores for any equal logins.
      -- If `filterToSecondList` then no elements in the first list 
      -- but not the second list will be kept.
      zipReviews : List (Score login) -> List (Score login) -> Op -> (filterToSecondList : Bool) -> List (Score login)
      zipReviews [] [] _ _        = []
      zipReviews [] ys Add _      = ys
      zipReviews [] ys Subtract _ = { combinedScore := 0 } <$> ys
      zipReviews xs [] _ True     = []
      zipReviews xs [] _ False    = xs
      zipReviews (x@(MkScore l1 s1 c1) :: xs) ys op filter =
        case (deleteBy' ((==) `on` user) x ys, filter) of
             ((Nothing      , ys'), False)      => (MkScore l1 s1 c1             ) :: zipReviews xs ys' op filter
             ((Nothing      , ys'), True )      =>                                    zipReviews xs ys' op filter
             ((Just (MkScore _ _ c2'), ys'), _) => (MkScore l1 s1 (c1 `calc` c2')) :: zipReviews xs ys' op filter
        where
          calc : Nat -> Nat -> Nat
          calc k j with (op)
            _ | Add      = k + j
            _ | Subtract = k `minus` j

||| Choose reviewers that maintain harmony in the review world.
||| Returns the logins of all reviewers that would work equally well.
|||
||| @ closedReviews   The logins of each reviewer of each closed PR (duplicates intact).
||| @ openReviews     The logins of each reviewer of each open PR (duplicates intact).
||| @ candidates      The logins of all potential reviewers that should be considered.
||| @ forcedReviewers The logins of all reviewers that are being forced (not chosen by harmony).
||| @ author          The author of the PR for which a reviewer is being picked.
export
chooseReviewers : Ord login =>
                  (closedReviews : List login)
               -> (openReviews : List login)
               -> (candidates : List login)
               -> (forcedReviewers : List login)
               -> (author : login)
               -> List (login, Nat)
chooseReviewers closedReviews openReviews candidates forcedReviewers author = 
  let remainingOptions = (nub candidates) \\ (author :: forcedReviewers)
      scoredOptions    = scoredReviewers closedReviews openReviews remainingOptions
  in  case scoredOptions of
           [] => []
           ((MkScore l s c) :: xs) => 
             let rest  = takeWhile ((== c) . combinedScore) xs 
                 rest' = loginScore <$> rest
             in  (l, c) :: rest'

export
randomReviewer : HasIO io => List (login, Nat) -> io (Maybe login)
randomReviewer [] = pure Nothing
randomReviewer (x :: xs) = (Just . fst) <$> rndSelect (x :: xs)

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

    graphOne : (highScore : Nat) -> (Score login) -> Doc AnsiStyle
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

    graph : (highScore : Nat) -> List (Score login) -> Doc AnsiStyle
    graph highScore = vsep . map (graphOne highScore)

