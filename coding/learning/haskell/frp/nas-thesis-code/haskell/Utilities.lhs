This file contains an assortment of utility functions.

> module Utilities where

> type List a = [ a ]

-- Combinators ---------------------------------------------------

See ArrowCombinators for many more routing combiators

> const2 :: a -> b -> c -> a
> const2 a _ _ = a

> result2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
> result2 g f a = g . f a

> result3 :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
> result3 g f a = result2 g (f a)

an example

third :: (c -> d) -> (a,(b,c)) -> (a,(b,d))
third =  second . second

--------Pairs-----------------------------------------------------

> pairZipWiths                 :: (a -> b -> c) -> (x -> y -> z) -> (a,x) -> (b,y) -> (c,z)
> pairZipWiths f g (a,x) (b,y) =  (f a b, g x y)

> mapPair :: (a -> b) -> (a,a) -> (b,b)
> mapPair f (a1,a2) = (f a1, f a2)

-- Lists ---------------------------------------------------------

> safehead         :: a -> [a] -> a
> safehead a []    =  a
> safehead _ (a:_) =  a

-- Maybe ---------------------------------------------------------

> maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
> maybeFilter p Nothing  = Nothing
> maybeFilter p (Just a) = if p a
>                           then Just a
>                           else Nothing

> maybeMerge :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
> maybeMerge fa fb fab Nothing Nothing   = Nothing
> maybeMerge fa fb fab Nothing (Just b)  = Just (fb b)
> maybeMerge fa fb fab (Just a) Nothing  = Just (fa a)
> maybeMerge fa fb fab (Just a) (Just b) = Just (fab a b)

> maybeMergeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
> maybeMergeWith f = maybeMerge id id f

-- Subtraction ---------------------------------------------------

-- useful for sections as (- n) parses as -n

> sub :: Num a => a -> a -> a
> sub =  (-)
