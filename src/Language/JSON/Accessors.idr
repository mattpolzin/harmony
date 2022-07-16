module Language.JSON.Accessors

import Data.Either
import Data.List
import Data.Vect
import Language.JSON

%default total

export
lookupAll : Vect n String -> List (String, JSON) -> Either String (Vect n JSON)
lookupAll [] dict            = Right []
lookupAll (key :: keys) dict = [| lookup' key dict :: lookupAll keys dict |]
  where
    lookup' : String -> List (String, a) -> Either String a
    lookup' key = maybeToEither "Missing required key: \{key}." . lookup key

export
optional : (JSON -> Either String a) -> JSON -> Either String (Maybe a)
optional f JNull = Right Nothing
optional f json = Just <$> (f json)

export
bool : JSON -> Either String Bool
bool (JBoolean x) = Right x
bool json = Left "Expected a bool but found \{show json}."

export
string : JSON -> Either String String
string (JString x) = Right x
string json = Left "Expected a string but found \{show json}."

export
integer : JSON -> Either String Integer
integer (JNumber x) = Right $ cast x
integer json = Left "Expected an integer but found \{show json}."

export
array : (JSON -> Either String a) -> JSON -> Either String (List a)
array f (JArray xs) = traverse f xs
array f json = Left "Expected an array but found \{show json}."

export
object : JSON -> Either String (List (String, JSON))
object (JObject xs) = Right xs
object json = Left "Expected an object but found \{show json}."

