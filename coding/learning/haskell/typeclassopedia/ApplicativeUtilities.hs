{-# LANGUAGE ApplicativeDo #-}

-- 1. Implement a function
-- sequenceAL :: Applicative f => [f a] -> f [a]
-- There is a generalized version of this, sequenceA, which works for any 
-- Traversable (see the later section on Traversable), but implementing this
-- version specialized to lists is a good exercise.

-- we can do it with ApplicativeDo but beware: this is not sequencing as we
-- know it from monads! The bindings run independent from each other 
-- conceptually in parallel and can be swapped because they dont depend
-- on each other - this is the idea behind an Applicative
sequenceALDo :: Applicative f => [f a] -> f [a]
sequenceALDo [] = pure []
sequenceALDo (f:fs) = do
  as <- sequenceALDo fs -- can be swapped ..
  a  <- f               -- .. with this
  pure (a:as)

-- This is basically a translation of the above into an applicative style
-- implementation. Base case is obvious: the empty list lifted into the 
-- applicative structure.
-- The recursive case lifts (:) into the applicative structure and then
-- simply applies the applicative computations through a recursive call
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL []     = pure []
sequenceAL (f:fs) = (:) <$> f <*> sequenceAL fs 
                  -- pure (:) <*> f <*> sequenceAL fs -- is equivalent
  -- this was my original idea but type-inference seems not be be able to do 
  -- that, so i needed to do it the beautiful elegant way of applicative style
  -- where
    -- ret = (<*>) (\a -> (<*>) (\as -> a:as) as) f
    -- as  = sequenceAL' fs

-- Deriving the type of (:) <$> f <*> sequenceAL fs step-by-step (because I
-- don't get it:

-- :t (<$>) :: Functor f => (a -> b) -> f a -> f b
-- :t (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Start
-- (:) <$> f <*> sequenceAL
--   Check some types :t (<$>) (:) :: Functor f => f a -> f ([a] -> [a])
--    we are using a function with 2 arguments (instead of only 1) thus
--    the result is a function! When we apply the next argument
--    we get a clearer type :t (:) <$> (Just 3) :: Num a => Maybe ([a] -> [a]) 
-- Now need to use <*> because we have a function in the applicative / 
-- functorial structure and we are lost with fmap because it doesn't work here.
-- Applicative <*> comes to a rescue: (:) <$> f <*> now allows to apply 
-- the second argument of the original (:) but within the applicative context