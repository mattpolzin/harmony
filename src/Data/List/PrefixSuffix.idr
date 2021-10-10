module Data.List.PrefixSuffix

import Data.List
import Decidable.Equality

%default total

||| Store a list as a suffix appended to a prefix.
public export
data PrefixSuffix : (prefixList : List a) -> (suffixList : List a) -> (0 fullList : List a) -> Type where
  Split : (prefixList : List a) -> (suffixList : List a) -> PrefixSuffix prefixList suffixList (prefixList ++ suffixList)

public export
prefixSuffixInjective : PrefixSuffix (x :: xs) suffixList (y :: ys) -> (x = y, PrefixSuffix xs suffixList ys)
prefixSuffixInjective (Split (x :: xs) suffixList) = (Refl, Split xs suffixList)

public export
prefixSuffixProp : PrefixSuffix prefixList suffixList fullList -> prefixList ++ suffixList = fullList
prefixSuffixProp (Split _ _) = Refl

public export
(.suffix) : {prefixList : List a} -> PrefixSuffix prefixList suffixList fullList -> List a
prefSuf.suffix = prefixList

-- NonEmpty prefix & empty fullList
export
Uninhabited (PrefixSuffix (x :: xs) suffixList []) where
  uninhabited (Split (x :: xs) suffixList) impossible

listPlusNonEmptyListNotEmpty : {xs : _} -> Not (xs ++ (y :: ys) = [])
listPlusNonEmptyListNotEmpty {xs = []} prf = absurd prf
listPlusNonEmptyListNotEmpty {xs = (x :: xs)} prf = absurd prf

-- NonEmpty suffix & empty fullList
export
{prefixList : _} -> Uninhabited (PrefixSuffix prefixList (x :: xs) []) where
  uninhabited prefSuf with (prefixSuffixProp prefSuf)
    uninhabited prefSuf | prf = listPlusNonEmptyListNotEmpty prf

prefixDifferentVoid : Not (x = y) -> Not (suffixList ** PrefixSuffix (x :: xs) suffixList (y :: ys))
prefixDifferentVoid contra (_ ** (Split (x :: xs) _)) = contra Refl

export
dropPrefix : DecEq a => 
             (prefixList : List a) 
          -> (fullList : List a) 
          -> Dec (suffixList ** PrefixSuffix prefixList suffixList fullList)
dropPrefix [] fullList = Yes (fullList ** Split [] fullList)
dropPrefix (x :: xs) [] = No (\(l ** r) => absurd r)
dropPrefix (x :: xs) (y :: ys) with (decEq x y)
  dropPrefix (x :: xs) (y :: ys) | (No contra) = No (prefixDifferentVoid contra)
  dropPrefix (x :: xs) (x :: ys) | (Yes Refl) with (dropPrefix xs ys)
    dropPrefix (x :: xs) (x :: ys) | (Yes Refl) | (No contra) =
      No (\(suffixList ** prf) => contra (suffixList ** snd $ prefixSuffixInjective prf))
    dropPrefix (x :: xs) (x :: (xs ++ zs)) | (Yes Refl) | (Yes (zs ** Split xs zs)) = 
      Yes (zs ** Split (x :: xs) zs)

