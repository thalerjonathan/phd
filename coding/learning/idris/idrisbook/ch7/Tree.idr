data Tree e = Empty
            | Node (Tree e) e (Tree e)

Functor Tree where
  map func Empty = Empty
  map func (Node l e r) = Node (map func l) (func e) (map func r)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node l e r) 
    = let leftFold  = foldr func acc l 
          rightFold = foldr func leftFold r in
          func e rightFold
