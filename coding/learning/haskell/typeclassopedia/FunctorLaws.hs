{-# LANGUAGE NoImplicitPrelude #-}

-- import additional bits and pieces to test
import Prelude (id, (+), (*), (.), Eq, (==), Show)

class Functor f where
  fmap :: (a -> b) -> f a -> f b


--------------------------------------------------------------------------------
-- 1. Although it is not possible for a Functor instance to satisfy the first 
-- Functor law but not the second (excluding undefined), the reverse is possible.
-- Give an example of a (bogus) Functor instance which satisfies the second law 
-- but not the first.

data Maybe a = Nothing | Just a deriving (Eq, Show)

-- This functor implementation of Maybe violates the 1st law but satisfies
-- the second law: simply return always the same - this will satisfy the
-- second law but now the first!
instance Functor Maybe where
  fmap _ _ = Nothing

-- Testing Law 1 (shoulw be violated)
-- fmap id (Just 1) == id (Just 1)
--   False
-- Testing Law 2 (should hold)
-- fmap ((+1) . (*2)) (Just 3) == (fmap (+1) . fmap (*2)) (Just 3)
--   True

--------------------------------------------------------------------------------
-- 2. Which laws are violated by the evil Functor instance for list shown above: 
-- both laws, or the first law alone? Give specific counterexamples.

-- Evil Functor instance
instance Functor [] where
  --fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x:xs) = g x : g x : fmap g xs

-- VIOLATES BOTH laws because duplicating each element when running fmap:
-- Law 1: fmap id == id
--  fmap id [1,2,3,4] == [1,1,2,2,3,3,4,4] -- each element will be duplicated 
--  id [1,2,3,4]      == [1,2,3,4]         -- no element duplicated
--   => violates Law 1

--- Law 2: fmap (g . h) == fmap g . fmap h
--    fmap ((+1) . (*2)) [1,2,3,4]      == [3,3,5,5,7,7,9,9] -- each element will be duplicated once
--    (fmap (+1) . fmap (*2)) [1,2,3,4] == [3,3,3,3,5,5,5,5,7,7,7,7,9,9,9,9] -- each element will be duplicated twice!
--      => violates Law 2 as well