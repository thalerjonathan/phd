import Control.Monad.State

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
treeLabel t = evalState (treeLabelWith t) [1..]