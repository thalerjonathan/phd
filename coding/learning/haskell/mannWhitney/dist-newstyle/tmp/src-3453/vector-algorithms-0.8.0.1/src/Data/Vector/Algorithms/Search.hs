{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Search
-- Copyright   : (c) 2009-2015 Dan Doel, 2015 Tim Baumann
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (bang patterns)
--
-- This module implements several methods of searching for indicies to insert
-- elements into a sorted vector.

module Data.Vector.Algorithms.Search
       ( binarySearch
       , binarySearchBy
       , binarySearchByBounds
       , binarySearchL
       , binarySearchLBy
       , binarySearchLByBounds
       , binarySearchR
       , binarySearchRBy
       , binarySearchRByBounds
       , binarySearchP
       , binarySearchPBounds
       , gallopingSearchLeftP
       , gallopingSearchLeftPBounds
       , gallopingSearchRightP
       , gallopingSearchRightPBounds
       , Comparison
       ) where

import Prelude hiding (read, length)

import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Generic.Mutable

import Data.Vector.Algorithms.Common (Comparison, midPoint)

-- | Finds an index in a given sorted vector at which the given element could
-- be inserted while maintaining the sortedness of the vector.
binarySearch :: (PrimMonad m, MVector v e, Ord e)
             => v (PrimState m) e -> e -> m Int
binarySearch = binarySearchBy compare
{-# INLINE binarySearch #-}

-- | Finds an index in a given vector, which must be sorted with respect to the
-- given comparison function, at which the given element could be inserted while
-- preserving the vector's sortedness.
binarySearchBy :: (PrimMonad m, MVector v e)
               => Comparison e -> v (PrimState m) e -> e -> m Int
binarySearchBy cmp vec e = binarySearchByBounds cmp vec e 0 (length vec)
{-# INLINE binarySearchBy #-}

-- | Given a vector sorted with respect to a given comparison function in indices
-- in [l,u), finds an index in [l,u] at which the given element could be inserted
-- while preserving sortedness.
binarySearchByBounds :: (PrimMonad m, MVector v e)
                     => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
binarySearchByBounds cmp vec e = loop
 where
 loop !l !u
   | u <= l    = return l
   | otherwise = do e' <- unsafeRead vec k
                    case cmp e' e of
                      LT -> loop (k+1) u
                      EQ -> return k
                      GT -> loop l     k
  where k = midPoint u l
{-# INLINE binarySearchByBounds #-}

-- | Finds the lowest index in a given sorted vector at which the given element
-- could be inserted while maintaining the sortedness.
binarySearchL :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m Int
binarySearchL = binarySearchLBy compare
{-# INLINE binarySearchL #-}

-- | Finds the lowest index in a given vector, which must be sorted with respect to
-- the given comparison function, at which the given element could be inserted
-- while preserving the sortedness.
binarySearchLBy :: (PrimMonad m, MVector v e)
                => Comparison e -> v (PrimState m) e -> e -> m Int
binarySearchLBy cmp vec e = binarySearchLByBounds cmp vec e 0 (length vec)
{-# INLINE binarySearchLBy #-}

-- | Given a vector sorted with respect to a given comparison function on indices
-- in [l,u), finds the lowest index in [l,u] at which the given element could be
-- inserted while preserving sortedness.
binarySearchLByBounds :: (PrimMonad m, MVector v e)
                      => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
binarySearchLByBounds cmp vec e = binarySearchPBounds p vec
 where p e' = case cmp e' e of LT -> False ; _ -> True
{-# INLINE binarySearchLByBounds #-}

-- | Finds the greatest index in a given sorted vector at which the given element
-- could be inserted while maintaining sortedness.
binarySearchR :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m Int
binarySearchR = binarySearchRBy compare
{-# INLINE binarySearchR #-}

-- | Finds the greatest index in a given vector, which must be sorted with respect to
-- the given comparison function, at which the given element could be inserted
-- while preserving the sortedness.
binarySearchRBy :: (PrimMonad m, MVector v e)
                => Comparison e -> v (PrimState m) e -> e -> m Int
binarySearchRBy cmp vec e = binarySearchRByBounds cmp vec e 0 (length vec)
{-# INLINE binarySearchRBy #-}

-- | Given a vector sorted with respect to the given comparison function on indices
-- in [l,u), finds the greatest index in [l,u] at which the given element could be
-- inserted while preserving sortedness.
binarySearchRByBounds :: (PrimMonad m, MVector v e)
                      => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
binarySearchRByBounds cmp vec e = binarySearchPBounds p vec
 where p e' = case cmp e' e of GT -> True ; _ -> False
{-# INLINE binarySearchRByBounds #-}

-- | Given a predicate that is guaraneteed to be monotone on the given vector,
-- finds the first index at which the predicate returns True, or the length of
-- the array if the predicate is false for the entire array.
binarySearchP :: (PrimMonad m, MVector v e) => (e -> Bool) -> v (PrimState m) e -> m Int
binarySearchP p vec = binarySearchPBounds p vec 0 (length vec)
{-# INLINE binarySearchP #-}

-- | Given a predicate that is guaranteed to be monotone on the indices [l,u) in
-- a given vector, finds the index in [l,u] at which the predicate turns from
-- False to True (yielding u if the entire interval is False).
binarySearchPBounds :: (PrimMonad m, MVector v e)
                    => (e -> Bool) -> v (PrimState m) e -> Int -> Int -> m Int
binarySearchPBounds p vec = loop
 where
 loop !l !u
   | u <= l    = return l
   | otherwise = unsafeRead vec k >>= \e -> if p e then loop l k else loop (k+1) u
  where k = midPoint u l
{-# INLINE binarySearchPBounds #-}

-- | Given a predicate that is guaranteed to be monotone on the vector elements
-- in order, finds the index at which the predicate turns from False to True.
-- The length of the vector is returned if the predicate is False for the entire
-- vector.
--
-- Begins searching at the start of the vector, in increasing steps of size 2^n.
gallopingSearchLeftP
  :: (PrimMonad m, MVector v e) => (e -> Bool) -> v (PrimState m) e -> m Int
gallopingSearchLeftP p vec = gallopingSearchLeftPBounds p vec 0 (length vec)
{-# INLINE gallopingSearchLeftP #-}

-- | Given a predicate that is guaranteed to be monotone on the vector elements
-- in order, finds the index at which the predicate turns from False to True.
-- The length of the vector is returned if the predicate is False for the entire
-- vector.
--
-- Begins searching at the end of the vector, in increasing steps of size 2^n.
gallopingSearchRightP
  :: (PrimMonad m, MVector v e) => (e -> Bool) -> v (PrimState m) e -> m Int
gallopingSearchRightP p vec = gallopingSearchRightPBounds p vec 0 (length vec)
{-# INLINE gallopingSearchRightP #-}

-- | Given a predicate that is guaranteed to be monotone on the indices [l,u) in
-- a given vector, finds the index in [l,u] at which the predicate turns from
-- False to True (yielding u if the entire interval is False).
-- Begins searching at l, going right in increasing (2^n)-steps.
gallopingSearchLeftPBounds :: (PrimMonad m, MVector v e)
                           => (e -> Bool)
                           -> v (PrimState m) e
                           -> Int -- ^ l
                           -> Int -- ^ u
                           -> m Int
gallopingSearchLeftPBounds p vec l u
  | u <= l    = return l
  | otherwise = do x <- unsafeRead vec l
                   if p x then return l else iter (l+1) l 2
 where
 binSearch = binarySearchPBounds p vec
 iter !i !j !_stepSize | i >= u - 1 = do
   x <- unsafeRead vec (u-1)
   if p x then binSearch (j+1) (u-1) else return u
 iter !i !j !stepSize = do
   x <- unsafeRead vec i
   if p x then binSearch (j+1) i else iter (i+stepSize) i (2*stepSize)
{-# INLINE gallopingSearchLeftPBounds #-}

-- | Given a predicate that is guaranteed to be monotone on the indices [l,u) in
-- a given vector, finds the index in [l,u] at which the predicate turns from
-- False to True (yielding u if the entire interval is False).
-- Begins searching at u, going left in increasing (2^n)-steps.
gallopingSearchRightPBounds :: (PrimMonad m, MVector v e)
                            => (e -> Bool)
                            -> v (PrimState m) e
                            -> Int -- ^ l
                            -> Int -- ^ u
                            -> m Int
gallopingSearchRightPBounds p vec l u
  | u <= l    = return l
  | otherwise = iter (u-1) (u-1) (-1)
 where
 binSearch = binarySearchPBounds p vec
 iter !i !j !_stepSize | i <= l = do
   x <- unsafeRead vec l
   if p x then return l else binSearch (l+1) j
 iter !i !j !stepSize = do
   x <- unsafeRead vec i
   if p x then iter (i+stepSize) i (2*stepSize) else binSearch (i+1) j
{-# INLINE gallopingSearchRightPBounds #-}
