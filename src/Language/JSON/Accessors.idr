module Language.JSON.Accessors

import Data.Either
import Data.List
import Data.Vect
import JSON.Parser
import JSON.Encoder

%default total

access : Show v => (tyName : String) -> (v -> Maybe ty) -> v -> Either String ty
access tyName f x = 
  maybeToEither "Expected a \{tyName} but found \{show x}." $
    f x

export
bool : JSON -> Either String Bool
bool = access "bool" getBoolean

export
integer : JSON -> Either String Integer
integer = access "integer" getInteger

export
string : JSON -> Either String String
string = access "string" getString

export
stringy : (desc : String) -> (String -> Maybe a) -> JSON -> Either String a
stringy d f (JString x) = case f x of
                            (Just y) => Right y
                            Nothing  => Left "Expected \{d} but found \{show x}."
stringy d f json = Left "Expected a string but found \{show json}."

export
optional : (JSON -> Either String a) -> JSON -> Either String (Maybe a)
optional f JNull = Right Nothing
optional f json = Just <$> (f json)

export
array : (JSON -> Either String a) -> JSON -> Either String (List a)
array f (JArray xs) = traverse f xs
array f json = Left "Expected an array but found \{show json}."

export
object : JSON -> Either String (List (String, JSON))
object (JObject xs) = Right xs
object json = Left "Expected an object but found \{show json}."

export
lookupAll : Vect n String -> List (String, JSON) -> Either String (Vect n JSON)
lookupAll [] dict            = Right []
lookupAll (key :: keys) dict = [| lookup' key dict :: lookupAll keys dict |]
  where
    lookup' : String -> List (String, a) -> Either String a
    lookup' key = maybeToEither "Missing required key: \{key}." . lookup key
