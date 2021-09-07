module Data.User

import Data.Either
import Data.Vect
import Language.JSON
import Language.JSON.Accessors

public export
record User where
  constructor MkUser
  login : String
  name  : String

%name Data.User.User user

export
Show User where
  show (MkUser login name) = show (login, name)

export
json : User -> JSON
json (MkUser login name) =
  JObject [
      ("login", JString login)
    , ("name" , JString name)
    ]

export
parseUser : JSON -> Either String User
parseUser (JObject user) = do [login, name] <- lookupAll ["login", "name"] user
                              l <- string login
                              n <- string name
                              pure $ MkUser {
                                  login = l
                                , name  = n
                                }
parseUser (JArray _) = Left "Expected config JSON to be an Object, not an array."
parseUser _          = Left "Expected config JSON to be an Object."

||| Parse a single user from a JSON String
export
parseUserString : String -> Either String User
parseUserString =
  (maybeToEither "Failed to parse JSON" . JSON.parse) >=> parseUser

||| Parse a list of users from a JSON String
export
parseUsersString : String -> Either String (List User)
parseUsersString =
  (maybeToEither "Failed to parse JSON" . JSON.parse) >=> array parseUser

