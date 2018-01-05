import Data.Vect

vectTake : Fin (S n) -> Vect (n+m) e -> Vect n e
vectTake FZ xs = ?bla_1
vectTake (FS n) (x :: xs) = x :: vectTake n xs

total
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            (Just idx) => let x = index idx xs
                                              y = index idx ys in
                                              Just $ x + y