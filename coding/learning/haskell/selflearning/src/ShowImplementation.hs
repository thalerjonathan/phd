data BTree a = Leaf a | Branch (BTree a) (BTree a)

showTree :: (Show a) => BTree a -> String
showTree (Leaf a) = show a
showTree (Branch l r) = "Branch Left: " ++ (showTree l) ++ "Branch Right: " ++ (showTree r)

constructTree :: (Num a) => BTree a
constructTree = (Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)) (Branch (Leaf 4) (Leaf 5)))
