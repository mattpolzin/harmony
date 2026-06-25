module Data.Team

import Data.Date
import Data.Either
import Data.List
import Data.Maybe
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

%default total

public export
data ReviewRequestDelegation = Disabled | RoundRobin | LoadBalanced

graphQlStr : ReviewRequestDelegation -> Maybe String
graphQlStr Disabled = Nothing
graphQlStr RoundRobin = Just "ROUND_ROBIN"
graphQlStr LoadBalanced = Just "LOAD_BALANCE"

parseReviewRequestDelegation : String -> Either String ReviewRequestDelegation
parseReviewRequestDelegation "ROUND_ROBIN" = Right RoundRobin
parseReviewRequestDelegation "LOAD_BALANCE" = Right LoadBalanced
parseReviewRequestDelegation str = Left "Failed to parse TeamReviewAssignmentAlgorithm. Expected either ROUND_ROBIN or LOAD_BALANCE. Found \{str}"

public export
record Team where
  constructor MkTeam
  graphQlId : String
  slug  : String
  name  : String
  description  : String
  createdAt  : Date
  updatedAt  : Date
  reviewRequestDelegation : ReviewRequestDelegation

%name Data.Team.Team team

export
Show Team where
  show (MkTeam graphQlId slug name description createdAt updatedAt reviewRequestDelegation) = slug

export
json : Team -> JSON
json (MkTeam graphQlId slug name description createdAt updatedAt reviewRequestDelegation) =
  let reviewRequestDelegation' = graphQlStr reviewRequestDelegation
  in JObject $ [
      ("graphql_id", JString graphQlId)
    , ("slug" , JString slug)
    , ("name" , JString name)
    , ("description" , JString description)
    , ("created_at" , JString $ show createdAt)
    , ("updated_at" , JString $ show updatedAt)
    , ("review_request_delegation_enabled" , JBool $ isJust reviewRequestDelegation')
    ] ++ (maybe [] (\s => [("review_request_delegation_algorithm", JString s)]) reviewRequestDelegation')

export
parseTeam : JSON -> Either String Team
parseTeam json = do
  team <- object json
  [graphQlIdStr, slugStr, nameStr, descriptionStr, createdAtStr, updatedAtStr, reviewRequestDelegationEnabledStr] <- lookupAll ["graphql_id", "slug", "name", "description", "created_at", "updated_at", "review_request_delegation_enabled"] team
  graphQlId <- string graphQlIdStr
  slug <- string slugStr
  name <- string nameStr
  description <- string descriptionStr
  createdAt <- parseDateTime =<< string createdAtStr
  updatedAt <- parseDateTime =<< string updatedAtStr
  reviewRequestDelegationEnabled <- bool reviewRequestDelegationEnabledStr
  reviewRequestDelegation <-
    if not reviewRequestDelegationEnabled
       then pure Disabled
       else do [algorithm] <- lookupAll ["review_request_delegation_algorithm"] team
               reviewRequestDelegation <- string algorithm
               parseReviewRequestDelegation reviewRequestDelegation

  pure $ MkTeam {
      graphQlId
    , slug
    , name
    , description
    , createdAt
    , updatedAt
    , reviewRequestDelegation
    }

||| Parse a single team from a JSON String
export
parseTeamString : String -> Either String Team
parseTeamString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseTeam

||| Parse a list of teams from a JSON String
export
parseTeamsString : String -> Either String (List Team)
parseTeamsString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parseTeam

