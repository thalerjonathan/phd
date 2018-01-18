data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc  : (rec : SnocList xs) -> SnocList (xs ++ [x])

{-
reverseSnoc : SnocList ty -> List ty
reverseSnoc Empty = []
reverseSnoc (Snoc xs x) = x :: reverseSnoc xs
-}

snocListHelp : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input} snoc (x :: xs) 
  = rewrite appendAssociative input [x] xs in 
            snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverse : List a -> List a
myReverse input = myReverseHelper input (snocList input)
  where
    myReverseHelper : (input : List a) -> SnocList input -> List a
    myReverseHelper [] Empty = []
    myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverseWith : List a -> List a
myReverseWith input with (snocList input)
  myReverseWith [] | Empty = []
  myReverseWith (xs ++ [x]) | (Snoc rec) = x :: myReverseWith xs | rec

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = True
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) 
      = x == y && isSuffix xs ys | xsrec | ysrec
