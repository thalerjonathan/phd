{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs      #-}
import Prelude (Applicative (..), Functor (..), Show, ($), map, zipWith, (+), id, Int, repeat)

-- 1. Implement an instance of Applicative for Maybe.
data Maybe a = Nothing | Just a deriving Show

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just

  (<*>) Nothing _         = Nothing
  (<*>) (Just f) Nothing  = Nothing
  (<*>) (Just f) (Just x) = Just (f x)


-- 2. Determine the correct definition of pure for the ZipList instance of 
-- Applicative - there is only one implementation that satisfies the law 
-- relating pure and (<*>)

newtype ZipList a = ZipList { getZipList :: [a] } deriving Show

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ map f xs -- can also use fmap instead of map

-- need to use repeat. if it is simply ZipList [a] it would not satisfy
-- the laws
instance Applicative ZipList where
  pure :: a -> ZipList a
  pure a = ZipList $ repeat a   -- exercise

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)