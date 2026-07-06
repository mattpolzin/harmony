module Util.String

import Data.String
import public Data.So

%default total

public export
data NonEmpty : String -> Type where
  IsNonEmpty : (cs : String) -> So (cs /= "") => NonEmpty cs

export
value : String.NonEmpty _ -> String
value (IsNonEmpty cs) = cs

-- We implement this with believe_me even though it can be implemented with
-- `choose` because doing it this way and public exporting it allows us to
-- reduce expressions containing `nonEmpty` at compile time.
public export
nonEmpty : (cs : String) -> Maybe (NonEmpty cs)
nonEmpty "" = Nothing
nonEmpty cs = Just $ IsNonEmpty cs @{believe_me Oh}

public export
isHashPrefix : String -> Bool
isHashPrefix str =
  ("#" `isPrefixOf` str) || ("\\#" `isPrefixOf` str)

public export
parseBool : String -> Maybe Bool
parseBool x with (toLower x)
  _ | "yes"   = Just True
  _ | "true"  = Just True
  _ | "no"    = Just False
  _ | "false" = Just False
  _ | _ = Nothing
