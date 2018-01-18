data ListLast : List a -> Type where
  Empty    : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty           => NonEmpty [] x
                          (NonEmpty ys y) => NonEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)
  where
    describeHelper : (input : List Int) -> (form : ListLast input) -> String
    describeHelper [] Empty = "Empty"
    describeHelper (xs ++ [x]) (NonEmpty xs x) 
      = "Non-empty, initial portion = " ++ show xs

describeListEndWith : List Int -> String
describeListEndWith input with (listLast input)
  describeListEndWith []          | Empty         = "Empty"
  describeListEndWith (xs ++ [x]) | NonEmpty xs x = "Non-empty, initial portion = " ++ show xs

myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs