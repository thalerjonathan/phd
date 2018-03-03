labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (value :: ys) (x :: xs) = (value, x) :: labelWith ys xs

label : List a -> List (Integer, a)
label xs = labelWith (iterate (+1) 0) xs
