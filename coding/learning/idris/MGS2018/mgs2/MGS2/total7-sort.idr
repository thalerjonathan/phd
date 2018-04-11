import Data.List.Views

{-
data SplitRec : List a -> Type where
     SplitRecNil : SplitRec []
     SplitRecOne : {x : a} -> SplitRec [x]
     SplitRecPair : {lefts, rights : List a} ->
                    (lrec : Lazy (SplitRec lefts)) ->
                    (rrec : Lazy (SplitRec rights)) ->
                    SplitRec (lefts ++ rights)
-}

mergeSort : List a -> List a
mergeSort xs with (splitRec xs)
  mergeSort xs | with_pat = ?xs_rhs
