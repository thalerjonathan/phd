occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item []        = 0
occurrences item (x :: xs) 
  = case item == x of
         False => occurrences item xs
         True  => 1 + occurrences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid  Solid  = True
  (==) Liquid Liquid = True
  (==) Gas    Gas    = True
  (==) _      _      = False

  (/=) x      y      = not (x == y)

data Tree e = Empty
            | Node (Tree e) e (Tree e)

Eq e => Eq (Tree e) where
  (==) Empty Empty = True
  (==) (Node l1 v1 r1) (Node l2 v2 r2) = v1 == v2 && l1 == l2 && r1 == r2
  (==) _ _ = False