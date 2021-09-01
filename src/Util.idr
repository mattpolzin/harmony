module Util

%default total

||| Delete the first element of a list that passes the
||| given predicate with the given `a`. Return both the
||| element that was deleted and the remaining list.
export
deleteBy' : (a -> a -> Bool) -> a -> List a -> (Maybe a, List a)
deleteBy' p x [] = (Nothing, [])
deleteBy' p x (y :: xs) =
  if p x y
     then (Just y, xs)
     else mapSnd (y ::) $ deleteBy' p x xs

