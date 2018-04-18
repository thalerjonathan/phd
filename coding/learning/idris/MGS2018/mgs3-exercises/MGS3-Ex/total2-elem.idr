data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : Elem x ys -> Elem x (y :: ys)

Beatles : List String
Beatles = ["John", "Paul", "George", "Ringo"]

georgeInBeatles : Elem "George" Beatles

peteNotInBeatles : Not (Elem "Pete" Beatles)

elemAppend : Elem x xs -> Elem x (xs ++ ys)
elemAppend Here = ?elemAppend_rhs_1
elemAppend (There x) = ?elemAppend_rhs_2

insertElem : Elem x (outer ++ inner) -> Elem x (outer ++ n :: inner)

notInEmpty : Elem x [] -> Void
notInEmpty Here impossible
notInEmpty (There _) impossible

neitherHereNorThere : ((x = y) -> Void) -> (Elem x xs -> Void) -> Elem x (y :: xs) -> Void
neitherHereNorThere f g Here = f Refl
neitherHereNorThere f g (There prf) = g prf

isElem : DecEq a => (x : a) -> (xs : List a) -> Dec (Elem x xs)
isElem x [] = No notInEmpty
isElem x (y :: xs) with (decEq x y)
  isElem y (y :: xs) | (Yes Refl) = Yes Here
  isElem x (y :: xs) | (No contra) = case isElem x xs of
                                          (Yes prf) => Yes (There prf)
                                          (No contra1) => No (neitherHereNorThere contra contra1)
