
data MyElem : a -> List a -> Type where
  Here  : MyElem x (x :: xs)
  There : (later : MyElem x xs) -> MyElem x (y :: xs)
-- I thought we needed something like this for lists:
-- Never : MyElem x []

data Last : List a -> a -> Type where
  LastOne  : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notInNilList : Last [] value -> Void
notInNilList LastOne impossible
notInNilList (LastCons _) impossible

-- had to look at solution here
lastNoMatch : (lastNotEq : (value = x) -> Void) -> Last [x] value -> Void
lastNoMatch lastNotEq LastOne = lastNotEq Refl
lastNoMatch _ (LastCons LastOne) impossible
lastNoMatch _ (LastCons (LastCons _)) impossible

lastNotInTail : (contra : Last (x :: xs) value -> Void) -> Last (y :: (x :: xs)) value -> Void
lastNotInTail contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNilList
isLast (x :: []) value = case decEq value x of
                              Yes Refl     => Yes LastOne
                              No lastNotEq => No (lastNoMatch lastNotEq)
isLast (y :: (x :: xs)) value = case isLast (x :: xs) value of
                                Yes prf   => Yes (LastCons prf)
                                No contra => No (lastNotInTail contra)
