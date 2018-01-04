import Tree

-- listToTree [1,4,3,5,2]
total
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- treeToList (listToTree [4,1,8,7,2,3,9,5,6])
total
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right
