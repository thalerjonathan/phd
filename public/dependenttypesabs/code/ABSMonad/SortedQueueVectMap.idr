module SortedQueueVectMap

import Data.Vect

%default total

-- A sorted queue, with a map concept which maps the
-- key (k) to a value (v) where the queue is sorted 
-- according to the keys.
-- Has the length of the queue in the type, as the
-- Vect has its length in the type => VectMap

export
data SortedQueueVectMap : (n : Nat) -> (k : Type) -> (v : Type) -> Type where
  Empty : SortedQueueVectMap Z k v
  Elem  : (key : k) -> (value : v) -> (tail : SortedQueueVectMap n k v) -> SortedQueueVectMap (S n) k v

export
(Show k, Show v) => Show (SortedQueueVectMap n k v) where
  show Empty = ""
  show (Elem key value tail) = "(" ++ show key ++ ", " ++ show value ++ ") " ++ show tail

export
empty : SortedQueueVectMap Z k v
empty = Empty

export
singleton : (key : k) -> (value : v) -> SortedQueueVectMap 1 k v 
singleton key value = Elem key value empty

||| Inserts the value descending according to key
export
insert : Ord k => 
         (key : k) ->
         (value : v) -> 
         SortedQueueVectMap n k v -> 
         SortedQueueVectMap (S n) k v 
insert key value Empty = Elem key value Empty
insert key value elem@(Elem key' value' tail) 
  = case compare key key' of
         LT => Elem key value elem
         _  => Elem key' value' (insert key value tail)

export
merge : Ord k => 
        SortedQueueVectMap n k v -> 
        SortedQueueVectMap m k v -> 
        SortedQueueVectMap (n + m) k v
merge Empty q = q
merge (Elem key value tail) q = 
    let q'  = insert key value q
        q'' = merge tail q'
    in  proveMergeTailSucc q''
  where
    -- NOTE: we don't need a 'sym' here, because: ?
    proveMergeTailSucc : SortedQueueVectMap (n + (S m)) k v -> SortedQueueVectMap (S (plus n m)) k v
    proveMergeTailSucc {n} {m} q = rewrite plusSuccRightSucc n m in q

export
first : SortedQueueVectMap n k v -> Maybe (k, v)
first Empty = Nothing
first (Elem key value tail) = Just (key, value)

export
first' : SortedQueueVectMap (S n) k v -> (k, v)
first' (Elem key value tail) = (key, value)

export
dropFirst : SortedQueueVectMap (S n) k v -> SortedQueueVectMap n k v
dropFirst (Elem key value tail) = tail

export
fromVect : Ord k => Vect n (k, v) -> SortedQueueVectMap n k v
fromVect [] = Empty
fromVect ((key, value) :: xs) =
  let q = fromVect xs 
  in  insert key value q

testVect1 : Vect 4 (Nat, String)
testVect1 = [(10, "Jonathan"), (2, "Dominik"), (1, "Rafael"), (7, "Wolfgang")]

testVect1' : Vect 4 (Nat, String)
testVect1' = [(42, "Irmi"), (32, "Ilse"), (1, "Josef"), (3, "Barbara")]

testVect2 : Vect 4 (Nat, (Nat, String))
testVect2 = [(10, (0, "Jonathan")), (2, (1, "Dominik")), (1, (2, "Rafael")), (7, (3, "Wolfgang"))]

testQueue : IO ()
testQueue = do
  let q = fromVect testVect2
  let firstElem = first' q

  putStrLn $ show firstElem
  putStrLn $ show q

  let q' = dropFirst q
  let firstElem' = first' q'
  putStrLn $ show firstElem'
  putStrLn $ show q'

  let q1 = fromVect testVect1
  let q2 = fromVect testVect1'
  let q3 = merge q1 q2
  putStrLn $ show q3