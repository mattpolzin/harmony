module Data.Pagination

import Data.Fin
import Data.Fin.Extra
import Data.Nat

%default total

||| A representation of all pages needed to cover the given number
||| of items with the given number of items per page.
|||
||| The last index is the current page (because a pagination can
||| represent the remaining items in another pagination with more
||| pages and more total items).
public export
data Pagination : (0 items : Nat) -> (0 perPage : Nat) -> (0 page : Nat) -> (contents : Type) -> Type where
  ||| A page and the page following it.
  NonTerminal : (page : Nat)
             -> {perPage : Nat}
             -> {remainder : Nat}
             -> perPage `LTE` ((perPage + (page * perPage)) + remainder)
             => perPage `GT` 0
             => (x : contents)
             -> (next : Pagination remainder perPage (S page) contents)
             -> Pagination (remainder + perPage) perPage page contents

  ||| The last page.
  Last : (page : Nat)
      -> {perPage : Nat}
      -> (lastPageSize : Nat)
      -> lastPageSize `LTE` perPage
      => perPage `GT` 0
      => lastPageSize `GT` 0
      => (x : contents)
      -> Pagination lastPageSize perPage page contents

%name Pagination pgs

||| A Pagination without any contents is very useful for describing
||| the shape of some pages (the number of pages, items per page,
||| offsets, etc.)
public export
PaginationShape : Nat -> Nat -> Nat -> Type
PaginationShape items perPage page = Pagination items perPage page ()

export
Uninhabited (Pagination 0 _ _ _) where
  uninhabited (Last _ _ _) impossible
  uninhabited (NonTerminal _ {perPage=0}     {remainder=0} @{_} @{perPageGTZ} _ _) = absurd perPageGTZ
  uninhabited (NonTerminal _ {perPage=(S k)} {remainder=0}                    _ _) impossible
  uninhabited (NonTerminal _                 {remainder=(S k)}                _ _) impossible

export
Uninhabited (Pagination _ 0 _ _) where
  uninhabited (Last _ @{_} @{perPageGTZ} _ _) = absurd perPageGTZ
  uninhabited (NonTerminal _ @{_} @{perPageGTZ} _ _) = absurd perPageGTZ

--
-- Accessors
--

||| Get the next page unless the current page is the last one.
public export
next : Pagination items perPage page contents -> Dec (Pagination (items `minus` perPage) perPage (S page) contents)
next (NonTerminal page {perPage} {remainder} x nextPage) =
  Yes $
    rewrite plusCommutative remainder perPage in
      rewrite minusPlus perPage {n=remainder} in
        nextPage
next (Last page items @{itemsLTEperPage} x) = 
  let zero = minusPlusZero items (perPage `minus` items)
      diff = plusMinusLte items perPage itemsLTEperPage
  in  No $
        rewrite sym diff in
          rewrite plusCommutative (perPage `minus` items) items in
            rewrite zero in
              absurd

||| Get the next page unless the current page is the last one.
public export
(.next) : Pagination items perPage page contents -> Dec (Pagination (items `minus` perPage) perPage (S page) contents)
(.next) = next

||| Get the offset (zero-indexed) of the given page.
public export
offset : Pagination _ _ _ _ -> Nat
offset (NonTerminal page {perPage} _ _) = page * perPage
offset (Last page {perPage} _ _) = page * perPage

||| Get the offset (zero-indexed) of the current page.
public export
(.offset) : Pagination _ _ _ _ -> Nat
(.offset) = offset

||| Get the (zero-indexed) index of the current page.
public export
idx : Pagination _ _ _ _ -> Nat
idx (NonTerminal k _ _) = k
idx (Last k _ _) = k

||| Get the (zero-indexed) index of the current page.
public export
(.idx) : Pagination _ _ _ _ -> Nat
(.idx) = idx

||| Get the size of the current page.
||| This is `perPage` for all pages except for
||| the last page.
public export
size : Pagination _ _ _ _ -> Nat
size (NonTerminal {perPage} _ _ _) = perPage
size (Last _ lastPageSize _) = lastPageSize

||| Get the size of the current page.
||| This is `perPage` for all pages except for
||| the last page.
public export
(.size) : Pagination _ _ _ _ -> Nat
(.size) = size

||| Get the number of pages in the pagination.
public export
length : Pagination _ _ _ _ -> Nat
length (Last _ _ _) = 1
length (NonTerminal _ _ next) = S (length next)

||| Get the page indices represented by the pagination.
public export
indices : Pagination _ _ _ _ -> List Nat
indices (Last k _ _) = [k]
indices (NonTerminal k _ next) = k :: indices next

--
-- Interface implementations
--

export
Show (Pagination _ _ _ _) where
  show (NonTerminal pg {perPage} _ next) =
    let offset = perPage * pg
    in  "page \{show  pg} : \{show  offset} -> \{show  (offset + (pred perPage))}\n"
     ++ show next
  show pg =
    "page \{show  pg.idx} : \{show  pg.offset} -> \{show  (pg.offset + (pred pg.size))}"

export
Functor (Pagination items perPage page) where
  map f (NonTerminal page x next) = NonTerminal page (f x) (map f next)
  map f (Last page items x) = Last page items (f x)

export
Foldable (Pagination items perPage page) where
  foldr f x (Last page items y) = f y x
  foldr f x (NonTerminal page y next) = f y (foldr f x next)

export
Traversable (Pagination items perPage page) where
  traverse f (NonTerminal page x next) =
    (\x',next' => NonTerminal page x' next') <$> f x <*> traverse f next
  traverse f (Last page items x) = (Last page items) <$> f x

||| A special traversal where the page metadata is passed to the function
||| being traversed with.
export
traverse' : Applicative f =>
            ((page : Nat) -> (perPage : Nat) -> (x : contents) -> f b)
         -> Pagination items perPage page contents 
         -> f (Pagination items perPage page b)
traverse' g (NonTerminal page {perPage} x next) =
  (\x',next' => NonTerminal page x' next') <$> g page perPage x <*> traverse' g next
traverse' g (Last page items x) = (Last page items) <$> g page perPage x

--
-- Functions that create PaginationShapes (pages & pages')
--

lemma' : {a,b : _} -> a `LTE` b -> (c ** c + a = b)
lemma' prf = (b `minus` a ** plusMinusLte a b prf)

lemma : {a,b : _} -> a `LT` b -> (c ** (c + a = b, c `GT` 0))
lemma (LTESucc x) with (lemma' x)
  lemma (LTESucc x) | (c' ** prf) =
    ((S c') ** (cong S prf, LTESucc LTEZero))

mutual
  nonTerminalPage : {remainder : Nat}
                 -> (page : Nat)
                 -> (remainingItems : Nat)
                 -> (perPage : Nat)
                 -> (0 remainingOk : remainder + perPage = remainingItems)
                 => perPage `GT` 0
                 => remainder `GT` 0
                 => PaginationShape remainingItems perPage page
  nonTerminalPage page remainingItems perPage with (sym remainingOk)
    nonTerminalPage page (remainder + perPage) perPage | Refl =
      let okPageSize : perPage `LTE` (perPage + (page * perPage)) + remainder = 
            rewrite sym $ plusAssociative perPage (page * perPage) remainder in lteAddRight perPage
      in NonTerminal page () @{okPageSize} $
           -- we know this is total because remainder is strictly less than remainingItems
           assert_total (pagesHelper (S page) remainder perPage)

  pagesHelper : (page : Nat)
        -> (remainingItems : Nat)
        -> (perPage : Nat)
        -> perPage `GT` 0
        => remainingItems `GT` 0
        => PaginationShape remainingItems perPage page
  pagesHelper page remainingItems perPage with (isLT perPage remainingItems)
    _ | (Yes prf)   = let (remainder ** (remainderOk, remainderGtZ)) = lemma prf
                      in  nonTerminalPage page remainingItems perPage @{remainderOk}
    _ | (No contra) = Last page remainingItems () @{notLTImpliesGTE contra}

||| Create a series of pages with a certain number of items on each
||| page such that the given number of total items all fit on one of
||| the pages.
|||
||| @items   The non-zero total number of items to page over.
||| @perPage The non-zero number of items to put on each page.
export
metaPages : (items : Nat)
         -> (perPage : Nat) 
         -> items `GT` 0 
         => perPage `GT` 0 
         => perPage `LTE` items
         => PaginationShape items perPage 0
metaPages items perPage = pagesHelper 0 items perPage

gtIsNonZero : a `GT` b -> NonZero a
gtIsNonZero (LTESucc x) = SIsNonZero

divNatNZLemma : (a,b : _) -> (prf : b `LTE` a) -> {0 prf' : NonZero b} -> divNatNZ a b prf' `GT` 0
divNatNZLemma 0 0 _ impossible
divNatNZLemma 0 (S k) prf = absurd prf
divNatNZLemma (S k) 0 _ impossible
divNatNZLemma (S k) (S j) prf with (lte (S k) j) proof prf''
  divNatNZLemma (S k) (S j) prf | False = LTESucc LTEZero
  divNatNZLemma (S k) (S j) prf | True with (LTEImpliesNotGT prf)
    divNatNZLemma (S k) (S j) prf | True | contra = absurd . contra . LTESucc $ lteReflectsLTE (S k) j prf''

||| Create a series of pages with at lest the given number of pages.
|||
||| The count of items per page is kept constant for all pages except
||| the last one, which means that not all page counts will be honored
||| precisely. That is why this function results in a pagination with
||| "at least the given number of pages."
|||
||| Page counts that evenly divide total items are predictably honored.
|||
||| @items The non-zero total number of items to page over.
||| @pages The minimum number of pages to produce.
export
metaPages' : (items : Nat)
          -> (pages : Nat)
          -> items `GT` 0
          => (0 pagesOk : pages `GT` 0)
          => pages `LTE` items
          => PaginationShape items (divNatNZ items pages (gtIsNonZero pagesOk)) 0
metaPages' items pages with (divNatNZ items pages (gtIsNonZero pagesOk)) proof prf
  metaPages' items pages | perPage =
    let prf' = divNatNZLemma items pages %search
    in  pagesHelper 0 items perPage @{replace {p=LTE 1} prf prf'}

{-
Not currently able to prove the following props reflexively.

namespace PagesProperties
  prop1 : pages 1 1 = the (Pagination 1 1 0) (Last 0 1)
  prop1 = ?prop1_rhs

  prop2 : pages 20 10 = the (Pagination 20 10 0) (NonTerminal 0 (Last 1 10))
  prop2 = ?prop2_rhs

  prop3 : pages 15 10 = the (Pagination 15 10 0) (NonTerminal 0 (Last 1 5))
  prop3 = ?prop3_rhs

namespace PagesPrimeProperties
  prop1 : pages' 2 1 = the (Pagination 2 2 0) (Last 0 2)
  prop1 = ?prop1_rhs

  prop2 : pages' 20 2 = the (Pagination 20 10 0) (NonTerminal 0 (Last 1 10))
  prop2 = ?prop2_rhs
-}

-- test : Pagination _ _ _ _ -> IO ()
-- test (NonTerminal page x next) = do
--   printLn (NonTerminal page x next)
--   test next
-- test pg = printLn pg

