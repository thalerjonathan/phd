listTail : List a -> Maybe (List a)



listInit : List a -> Maybe (List a)





data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocList : (xs : List a) -> SnocList xs


myReverse : List a -> List a
