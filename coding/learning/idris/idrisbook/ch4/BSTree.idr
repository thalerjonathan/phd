data BSTree : Type -> Type where
     Empty : Ord e => BSTree e
     Node : Ord e => (left : BSTree e) -> (val : e) -> (right : BSTree e) -> BSTree e

total
insert : e -> BSTree e -> BSTree e
insert x Empty = Node Empty x Empty
insert x t@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => t
                                      GT => Node left val (insert x right)