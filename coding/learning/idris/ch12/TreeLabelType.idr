data State : (st : Type) -> Type -> Type where
  Get : State st st
  Put : st -> State st ()

  Pure : t -> State st t
  Bind : State st a -> (a -> State st b) -> State st b

(>>=) : State st a -> (a -> State st b) -> State st b
(>>=) = Bind

get : State st st
get = Get

put : st -> State st ()
put = Put

pure : t -> State st t
pure = Pure

runState : State st a -> st -> (a, st)
runState Get st = (st, st)
runState (Put st) st0 = ((), st)
runState (Pure a) st = (a, st)
runState (Bind stAct stCont) st0
  = let (a, st') = runState stAct st0 in
        runState (stCont a) st'

data Tree e = Empty
            | Node (Tree e) e (Tree e)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree e -> List e
flatten Empty = []
flatten (Node left e right) = flatten left ++ [e] ++ flatten right

treeLabelWith : Tree e -> State (Stream l) (Tree (l, e))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left e right) = do
  leftLabeled <- treeLabelWith left
  (l :: ls) <- get
  put ls
  rightLabeled <- treeLabelWith right
  pure $ Node leftLabeled (l, e) rightLabeled

treeLabel : Tree e -> Tree (Integer, e)
treeLabel t = fst (runState (treeLabelWith t) [1..])