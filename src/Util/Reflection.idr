module Util.Reflection

import Language.Reflection

%default total

public export
record Clause where
  constructor MkClause
  lhs : Name
  rhs : TTImp

||| Useful for elaborating basic properties where you want to know that
||| a function is either covering or onto w/r/t a datatype.
||| See e.g. `Data.Config.SettablePropNamedProperties`
|||
||| case `ofParam` of
|||      `clauseFn <$> cases`
public export
elabCase : (ofParam : TTImp) -> (ty : TTImp) -> (cases : List Name) -> (clauseFn : Name -> Util.Reflection.Clause) -> TTImp
elabCase ofParam ty cases clauseFn =
  ICase EmptyFC [] ofParam ty $ (\c => PatClause EmptyFC (IVar EmptyFC c.lhs) c.rhs) . clauseFn <$> cases

