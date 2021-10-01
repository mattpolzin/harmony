module Data.Review

import Data.Date
import Data.Either
import Language.JSON
import Language.JSON.Accessors
import Data.Vect

public export
data State = Approved | Commented | ChangesRequested | Dismissed

public export
record Review where
  constructor MkReview
  submittedAt : Date
  author      : String
  state       : State

export
isAuthor : String -> Review -> Bool
isAuthor login = (== login) . author

parseState : String -> Either String State
parseState "APPROVED"          = Right Approved
parseState "COMMENTED"         = Right Commented
parseState "CHANGES_REQUESTED" = Right Approved
parseState "DISMISSED"         = Right Approved
parseState _ = Left "Failed to parse Review State."

parseDateTime : String -> Either String Date
parseDateTime = maybeToEither "Failed to parse Date" . parseDateTimeString

export
parseReview : JSON -> Either String Review
parseReview json =
 do review <- object json
    [authorLogin, stateStr, submittedAtStr] <- lookupAll ["author", "state", "submitted_at"] review
    author      <- string authorLogin
    state       <- parseState    =<< string stateStr
    submittedAt <- parseDateTime =<< string submittedAtStr
    pure $ MkReview {
        submittedAt
      , author
      , state
      }

||| Parse a single review from a JSON String
export
parseReviewString : String -> Either String Review
parseReviewString =
  (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parseReview

||| Parse a list of reviews from a JSON String
export
parseReviewsString : String -> Either String (List Review)
parseReviewsString =
  (maybeToEither "Failed to parse JSON" . JSON.parse) >=> array parseReview
