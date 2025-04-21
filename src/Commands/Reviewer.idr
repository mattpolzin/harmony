module Commands.Reviewer

import Data.List
import Data.List1
import Data.Nat
import Data.String
import Data.ReviewScore

import System.Random
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Symbols

%default total

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
