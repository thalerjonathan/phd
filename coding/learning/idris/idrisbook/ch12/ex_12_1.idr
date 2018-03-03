import Control.Monad.State

update : (stateType -> stateType) -> State stateType ()
update f = do
  s <- get
  put (f s)

increase : Nat -> State Nat ()
increase inc = update (+inc)

data Tree e = Empty
            | Node (Tree e) e (Tree e)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

countEmpty : Tree e -> State Nat ()
countEmpty Empty = increase 1
countEmpty (Node left e right) = do
  countEmpty left
  countEmpty right

countEmptyNode : Tree e -> State (Nat, Nat) ()
countEmptyNode Empty = update (\(empty, node) => (empty + 1, node))
countEmptyNode (Node left e right) = do
  countEmptyNode left
  countEmptyNode right
  update (\(empty, node) => (empty, node + 1)) -- ordering of stateful commands does not matter in this case due to commutativity of addition