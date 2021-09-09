module Reviewer

import Data.Nat
import Data.String
import Data.List
import Data.List1
import System.Random.Node
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Util

%default total

||| Get scored reviewers sorted from lowest to highest score.
||| A low score indicates less review work (a better candidate for
||| assigning a new PR to).
scoredReviewers : Ord login =>
                  (closedReviews : List login)
               -> (openReviews : List login)
               -> (candidates : List login)
               -> List (login, Nat)
scoredReviewers closedReviews openReviews candidates =
  let closedReviewsWeighted = weightReviews 1 closedReviews
      openReviewsWeighted   = weightReviews 2 openReviews
      allReviews            = zipReviews closedReviewsWeighted openReviewsWeighted False
  in sort' $ zipReviews allReviews (weightReviews 0 candidates) True
  where
      -- The number of appearances is multiplied by the supplied weight which 
      -- allows us to weight some lists more heavily than others.
      weightReviews : (weight : Nat) -> List login -> List (login, Nat)
      weightReviews weight reviews =
        let grouped = groupAllWith id reviews
        in  grouped <&> (\xs => (head xs, (* weight) . length $ forget xs))

      -- Sort logins by the number of times each login was requested for review.
      sort' : List (login, Nat) -> List (login, Nat)
      sort' = sortBy $ compare `on` snd

      -- Add the scores together for any equal logins.
      -- If `filterToSecondList` then no elements in the first list 
      -- but not the second list will be kept.
      zipReviews : List (login, Nat) -> List (login, Nat) -> (filterToSecondList : Bool) -> List (login, Nat)
      zipReviews [] [] _     = []
      zipReviews [] ys _     = ys
      zipReviews xs [] True  = []
      zipReviews xs [] False = xs
      zipReviews (x@(l1, s1) :: xs) ys filter =
        case (deleteBy' ((==) `on` fst) x ys, filter) of
             ((Nothing      , ys'), False) => (l1, s1      ) :: zipReviews xs ys' filter
             ((Nothing      , ys'), True ) =>                   zipReviews xs ys' filter
             ((Just (_, s2'), ys'), _    ) => (l1, s1 + s2') :: zipReviews xs ys' filter

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
           []               => []
           (x@(l, s) :: xs) => x :: takeWhile ((== s) . snd) xs

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
           ((_, highScore) :: _) => graph (if highScore > 0 then highScore else 1) scoredOptions
  where
    graphOne : (highScore : Nat) -> login -> Nat -> Doc AnsiStyle
    graphOne highScore user score =
      let idt    = highScore `minus` score
          bar    = indent (cast idt) . pretty $ replicate score '#' 
          user   = annotate italic $ pretty user
      in  bar <++> user

    graph : (highScore : Nat) -> List (login, Nat) -> Doc AnsiStyle
    graph highScore = vsep . map (uncurry $ graphOne highScore)

