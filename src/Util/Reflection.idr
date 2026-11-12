module Util.Reflection

import Language.Reflection

%default total

public export
record Clause a where
  constructor MkClause
  lhs : a
  rhs : TTImp

||| Useful for elaborating basic properties where you want to know that
||| a function is either covering or onto w/r/t a datatype.
||| See e.g. `Data.Config.SettablePropNamedProperties`
|||
||| case `ofParam` of
|||      `clauseFn <$> cases`
public export
elabCase : (ofParam : TTImp) -> (ty : TTImp) -> (cases : List Name) -> (clauseFn : Name -> Util.Reflection.Clause Name) -> TTImp
elabCase ofParam ty cases clauseFn =
  ICase EmptyFC [] ofParam ty $ (\c => PatClause EmptyFC (IVar EmptyFC c.lhs) c.rhs) . clauseFn <$> cases

||| Provide cases for each of a set of values and use a fallback if none of those match.
public export
elabCase' : (ofParam : TTImp) -> (ty : TTImp) -> (cases : List a) -> (clauseFn : a -> Util.Reflection.Clause TTImp) -> (fallback : Clause) -> TTImp
elabCase' ofParam ty cases clauseFn fallback =
  let knownCases = (\c => PatClause EmptyFC c.lhs c.rhs) . clauseFn <$> cases
  in  ICase EmptyFC [] ofParam ty (knownCases ++ [fallback])

