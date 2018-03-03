data Tree e = Empty
            | Node (Tree e) e (Tree e)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree e -> List e
flatten Empty = []
flatten (Node left e right) = flatten left ++ [e] ++ flatten right

treeLabelWith : Stream l -> Tree e -> (Stream l, Tree (l, e))
treeLabelWith ls Empty = (ls, Empty)
treeLabelWith ls (Node left e right) 
  =  let (l :: lleft, treeleft) = treeLabelWith ls left
         (lright, treeRight) = treeLabelWith lleft right in
         (lright, Node treeleft (l, e) treeRight)

treeLabel : Tree e -> Tree (Integer, e)
treeLabel t = snd (treeLabelWith [1..] t)