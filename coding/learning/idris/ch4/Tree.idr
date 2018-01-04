module Tree

public export
data Tree e = Empty
            | Node (Tree e) e (Tree e)

%name Tree tree, tree1

export
total
insert : Ord e => e -> Tree e -> Tree e
insert x Empty = Node Empty x Empty
insert x t@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => t
                                      GT => Node left val (insert x right)