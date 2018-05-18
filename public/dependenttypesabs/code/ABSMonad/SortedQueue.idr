module SortedQueue

import Data.Vect

%default total

export
data SortedQueue : (n : Nat) -> (k : Type) -> (v : Type) -> Type where
  Empty : SortedQueue Z k v
  Elem  : (key : k) -> (value : v) -> (tail : SortedQueue n k v) -> SortedQueue (S n) k v

export
(Show k, Show v) => Show (SortedQueue n k v) where
  show Empty = ""
  show (Elem key value tail) = "(" ++ show key ++ ", " ++ show value ++ ") " ++ show tail

||| Inserts the value descending according to key
export
insert : Ord k => 
         (key : k) ->
         (value : v) -> 
         SortedQueue n k v -> 
         SortedQueue (S n) k v 
insert key value Empty = Elem key value Empty
insert key value elem@(Elem key' value' tail) 
  = case compare key key' of
         LT => Elem key value elem
         _  => Elem key' value' (insert key value tail)

export
first : SortedQueue (S n) k v -> (k, v)
first (Elem key value tail) = (key, value)

export
dropFirst : SortedQueue (S n) k v -> SortedQueue n k v
dropFirst (Elem key value tail) = tail

export
fromVect : Ord k => Vect n (k, v) -> SortedQueue n k v
fromVect [] = Empty
fromVect ((key, value) :: xs) =
  let q = fromVect xs 
  in  insert key value q

testVect : Vect 4 (Nat, String)
testVect = [(10, "Jonathan"), (2, "Dominik"), (1, "Rafael"), (7, "Wolfgang")]

testQueue : IO ()
testQueue = do
  let q = fromVect testVect
  let firstElem = first q

  putStrLn $ show firstElem
  putStrLn $ show q

  let q' = dropFirst q
  let firstElem' = first q'
  putStrLn $ show firstElem'
  putStrLn $ show q'