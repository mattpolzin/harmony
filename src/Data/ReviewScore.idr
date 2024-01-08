module Data.ReviewScore

import Data.List
import Data.List1
import Data.List.DeleteBy

%default total

public export
record ReviewScore login where
  constructor MkScore
  user          : login
  partialScore  : Nat
  combinedScore : Nat

export
loginScore : ReviewScore login -> (login, Nat)
loginScore (MkScore l s c) = (l, c)

||| Get scored reviewers sorted from lowest to highest score.
||| A low score indicates less review work (a better candidate for
||| requesting review from on a new PR).
|||
||| The resulting `Score` contains both a partial score (the score
||| based only on open reivews) and also a combined score (the score
||| after negatively weighting closed reviews).
|||
||| Closed reviews are negatively scored because they are review requests
||| that were not answered.
export
scoredReviewers : Ord login =>
                  (closedReviews : List login)
               -> (openReviews : List login)
               -> (candidates : List login)
               -> List (ReviewScore login)
scoredReviewers closedReviews openReviews candidates =
  let closedReviewsWeighted = weightReviews 1 closedReviews
      openReviewsWeighted   = weightReviews 4 openReviews
      allReviews            = zipReviews openReviewsWeighted closedReviewsWeighted Subtract False
  in sort' $ zipReviews allReviews (weightReviews 0 candidates) Add True
  where
      -- The number of appearances is multiplied by the supplied weight which 
      -- allows us to weight some lists more heavily than others.
      weightReviews : (weight : Nat) -> List login -> List (ReviewScore login)
      weightReviews weight reviews =
        let grouped = groupAllWith id reviews
        in  grouped <&> (\xs => let score = (* weight) . length $ forget xs in (MkScore (head xs) score score))

      -- Sort logins by the number of times each login was requested for review.
      sort' : List (ReviewScore login) -> List (ReviewScore login)
      sort' = sortBy $ compare `on` combinedScore

      data Op = Add | Subtract

      -- Add or subtract the scores for any equal logins.
      -- If `filterToSecondList` then no elements in the first list 
      -- but not the second list will be kept.
      zipReviews : List (ReviewScore login) -> List (ReviewScore login) -> Op -> (filterToSecondList : Bool) -> List (ReviewScore login)
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

