module Data.Date

import Data.List1
import Data.String

%default total

public export
record Date where
  constructor MkDate
  year, month, day : Nat

export
Show Date where
  show (MkDate year month day) = "\{show year}-\{pad month}-\{pad day}"
    where
      pad : Nat -> String
      pad k = if k < 10
                 then "0\{show k}"
                 else show k

export
Eq Date where
  (MkDate year month day) == (MkDate year' month' day') =
    year == year' && month == month' && day == day'

export
Ord Date where
  compare (MkDate year month day) (MkDate year' month' day') =
    compare [year, month, day] [year', month', day']

parsePositiveReversed : List Char -> Maybe Nat
parsePositiveReversed [] = Just 0
parsePositiveReversed (x :: xs) = [ n + 10 * !(parsePositiveReversed xs) | n <- parseNat x ]
  where
    parseNat : Char -> Maybe Nat
    parseNat '0' = Just 0
    parseNat '1' = Just 1
    parseNat '2' = Just 2
    parseNat '3' = Just 3
    parseNat '4' = Just 4
    parseNat '5' = Just 5
    parseNat '6' = Just 6
    parseNat '7' = Just 7
    parseNat '8' = Just 8
    parseNat '9' = Just 9
    parseNat _   = Nothing

export
parseDateString : String -> Maybe Date
parseDateString = guardSuccess . foldl go (Start ** ()) . unpack
  where
    data PrimaryPart = Year | Month

    data Part = Start
              | ||| Primary Part
                P PrimaryPart
              | ||| Separator (dash) following given primary part.
                S PrimaryPart
              | Day
              | Fail

    Partial : Part -> Type
    Partial Start     = ()
    Partial (P Year)  = List Char             -- partial year
    Partial (S Year)  = Nat                   -- full year
    Partial (P Month) = (Nat, List Char)      -- partial month
    Partial (S Month) = (Nat, Nat)            -- full month
    Partial Day       = (Nat, Nat, List Char) -- partial day
    Partial Fail      = ()

    -- if we are on day, parse it as a number and construct a
    -- Date or fail.
    guardSuccess : (part ** Partial part) -> Maybe Date
    guardSuccess (Day ** (y, m, cs)) = MkDate y m <$> parsePositiveReversed cs
    guardSuccess _ = Nothing

    go : (part ** Partial part) -> Char -> (part ** Partial part)
    go (Fail ** ()) c = (Fail ** ())

    -- kick things off with a digit
    go (Start ** ()) c = if isDigit c then (P Year ** [c]) else (Fail ** ())

    -- collect until you find a dash, then reverse and parse as number
    go ((P Year) ** cs) '-' = case parsePositiveReversed cs of
                                Nothing  => (Fail ** ())
                                (Just y) => (S Year ** y)
    go ((P Year) ** cs) c = (P Year ** c :: cs)

    -- kick things off with a digit
    go ((S Year) ** snd) c = if isDigit c then (P Month ** (snd, [c])) else (Fail ** ())

    -- collect until you find a dash, then reverse and parse as number
    go ((P Month) ** (y, cs)) '-' = case parsePositiveReversed cs of
                                         Nothing  => (Fail ** ())
                                         (Just m) => (S Month ** (y, m))
    go ((P Month) ** (y, cs)) c = (P Month ** (y, c :: cs))

    -- kick things off with a digit
    go ((S Month) ** (y, m)) c = if isDigit c then (Day ** (y, m, [c])) else (Fail ** ())

    -- collect until you are done
    go (Day ** (y, m, cs)) c = (Day ** (y, m, c :: cs))

export
parseDateTimeString : String -> Maybe Date
parseDateTimeString = parseDateString . head . split (== 'T')

