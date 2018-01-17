import Data.Vect

{-
removeElemNaive : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
removeElemNaive value (x :: xs) = case decEq x value of
                                  Yes prf   => xs
                                  No contra => x :: removeElem value xs
-}

export
removeElem : (value : a) -> 
             (xs : Vect (S n) a) ->
             (prf : Elem value xs) -> 
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: removeElem value ys later

export
removeElem_auto : (value : a) -> 
                  (xs : Vect (S n) a) ->
                  {auto prf : Elem value xs} -> 
                  Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf             