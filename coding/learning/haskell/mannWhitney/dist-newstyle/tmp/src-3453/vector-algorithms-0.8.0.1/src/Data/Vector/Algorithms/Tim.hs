{-# LANGUAGE BangPatterns #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Tim
-- Copyright   : (c) 2013-2015 Dan Doel, 2015 Tim Baumann
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (bang patterns)
--
-- Timsort is a complex, adaptive, bottom-up merge sort. It is designed to
-- minimize comparisons as much as possible, even at some cost in overhead.
-- Thus, it may not be ideal for sorting simple primitive types, for which
-- comparison is cheap. It may, however, be significantly faster for sorting
-- arrays of complex values (strings would be an example, though an algorithm
-- not based on comparison would probably be superior in that particular
-- case).
--
-- For more information on the details of the algorithm, read on.
--
-- The first step of the algorithm is to identify runs of elements. These can
-- either be non-decreasing or strictly decreasing sequences of elements in
-- the input. Strictly decreasing sequences are used rather than
-- non-increasing so that they can be easily reversed in place without the
-- sort becoming unstable.
--
-- If the natural runs are too short, they are padded to a minimum value. The
-- minimum is chosen based on the length of the array, and padded runs are put
-- in order using insertion sort. The length of the minimum run size is
-- determined as follows:
--
--   * If the length of the array is less than 64, the minimum size is the
--     length of the array, and insertion sort is used for the entirety
--
--   * Otherwise, a value between 32 and 64 is chosen such that N/min is
--     either equal to or just below a power of two. This avoids having a
--     small chunk left over to merge into much larger chunks at the end.
--
-- This is accomplished by taking the the mininum to be the lowest six bits
-- containing the highest set bit, and adding one if any other bits are set.
-- For instance:
--
--     length: 00000000 00000000 00000000 00011011 = 25
--     min:    00000000 00000000 00000000 00011011 = 25
--
--     length: 00000000 11111100 00000000 00000000 = 63 * 2^18
--     min:    00000000 00000000 00000000 00111111 = 63
--
--     length: 00000000 11111100 00000000 00000001 = 63 * 2^18 + 1
--     min:    00000000 00000000 00000000 01000000 = 64
--
-- Once chunks can be produced, the next step is merging them. The indices of
-- all runs are stored in a stack. When we identify a new run, we push it onto
-- the stack. However, certain invariants are maintained of the stack entries.
-- Namely:
--
--   if stk = _ :> z :> y :> x
--     length x + length y < length z
--
--   if stk = _ :> y :> x
--     length x < length y
--
-- This ensures that the chunks stored are decreasing, and that the chunk
-- sizes follow something like the fibonacci sequence, ensuring there at most
-- log-many chunks at any time. If pushing a new chunk on the stack would
-- violate either of the invariants, we first perform a merge.
--
-- If length x + length y >= length z, then y is merged with the smaller of x
-- and z (if they are tied, x is chosen, because it is more likely to be
-- cached). If, further,  length x >= length y then they are merged. These steps
-- are repeated until the invariants are established.
--
-- The last important piece of the algorithm is the merging. At first, two
-- chunks are merged element-wise. However, while doing so, counts are kept of
-- the number of elements taken from one chunk without any from its partner. If
-- this count exceeds a threshold, the merge switches to searching for elements
-- from one chunk in the other, and copying chunks at a time. If these chunks
-- start falling below the threshold, the merge switches back to element-wise.
--
-- The search used in the merge is also special. It uses a galloping strategy,
-- where exponentially increasing indices are tested, and once two such indices
-- are determined to bracket the desired value, binary search is used to find
-- the exact index within that range. This is asymptotically the same as simply
-- using binary search, but is likely to do fewer comparisons than binary search
-- would.
--
-- One aspect that is not yet implemented from the original Tim sort is the
-- adjustment of the above threshold. When galloping saves time, the threshold
-- is lowered, and when it doesn't, it is raised. This may be implemented in the
-- future.

module Data.Vector.Algorithms.Tim
       ( sort
       , sortBy
       ) where

import Prelude hiding (length, reverse)

import Control.Monad.Primitive
import Control.Monad (when)
import Data.Bits

import Data.Vector.Generic.Mutable

import Data.Vector.Algorithms.Search ( gallopingSearchRightPBounds
                                     , gallopingSearchLeftPBounds
                                     )
import Data.Vector.Algorithms.Insertion (sortByBounds', Comparison)

-- | Sorts an array using the default comparison.
sort :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m ()
sort = sortBy compare
{-# INLINABLE sort #-}

-- | Sorts an array using a custom comparison.
sortBy :: (PrimMonad m, MVector v e)
       => Comparison e -> v (PrimState m) e -> m ()
sortBy cmp vec
  | mr == len = iter [0] 0 (error "no merge buffer needed!")
  | otherwise = new 256 >>= iter [] 0
 where
 len = length vec
 mr = minrun len
 iter s i tmpBuf
   | i >= len  = performRemainingMerges s tmpBuf
   | otherwise = do (order, runLen) <- nextRun cmp vec i len
                    when (order == Descending) $
                      reverse $ unsafeSlice i runLen vec
                    let runEnd = min len (i + max runLen mr)
                    sortByBounds' cmp vec i (i+runLen) runEnd
                    (s', tmpBuf') <- performMerges (i : s) runEnd tmpBuf
                    iter s' runEnd tmpBuf'
 runLengthInvariantBroken a b c i = (b - a <= i - b) || (c - b <= i - c)
 performMerges [b,a] i tmpBuf
   | i - b >= b - a = merge cmp vec a b i tmpBuf >>= performMerges [a] i
 performMerges (c:b:a:ss) i tmpBuf
   | runLengthInvariantBroken a b c i =
     if i - c <= b - a
       then merge cmp vec b c i tmpBuf >>= performMerges (b:a:ss) i
       else do tmpBuf' <- merge cmp vec a b c tmpBuf
               (ass', tmpBuf'') <- performMerges (a:ss) c tmpBuf'
               performMerges (c:ass') i tmpBuf''
 performMerges s _ tmpBuf = return (s, tmpBuf)
 performRemainingMerges (b:a:ss) tmpBuf =
   merge cmp vec a b len tmpBuf >>= performRemainingMerges (a:ss)
 performRemainingMerges _ _ = return ()
{-# INLINE sortBy #-}

-- | Computes the minimum run size for the sort. The goal is to choose a size
-- such that there are almost if not exactly 2^n chunks of that size in the
-- array.
minrun :: Int -> Int
minrun n0 = (n0 `unsafeShiftR` extra) + if (lowMask .&. n0) > 0 then 1 else 0
 where
 -- smear the bits down from the most significant bit
 !n1 = n0 .|. unsafeShiftR n0 1
 !n2 = n1 .|. unsafeShiftR n1 2
 !n3 = n2 .|. unsafeShiftR n2 4
 !n4 = n3 .|. unsafeShiftR n3 8
 !n5 = n4 .|. unsafeShiftR n4 16
 !n6 = n5 .|. unsafeShiftR n5 32

 -- mask for the bits lower than the 6 highest bits
 !lowMask = n6 `unsafeShiftR` 6

 !extra = popCount lowMask
{-# INLINE minrun #-}

data Order = Ascending | Descending deriving (Eq, Show)

-- | Identify the next run (that is a monotonically increasing or strictly
-- decreasing sequence) in the slice [l,u) in vec. Returns the order and length
-- of the run.
nextRun :: (PrimMonad m, MVector v e)
        => Comparison e
        -> v (PrimState m) e
        -> Int -- ^ l
        -> Int -- ^ u
        -> m (Order, Int)
nextRun _ _ i len | i+1 >= len = return (Ascending, 1)
nextRun cmp vec i len = do x <- unsafeRead vec i
                           y <- unsafeRead vec (i+1)
                           if x `gt` y then desc y 2 else asc  y 2
 where
 gt a b = cmp a b == GT
 desc _ !k | i + k >= len = return (Descending, k)
 desc x !k = do y <- unsafeRead vec (i+k)
                if x `gt` y then desc y (k+1) else return (Descending, k)
 asc _ !k | i + k >= len = return (Ascending, k)
 asc x !k = do y <- unsafeRead vec (i+k)
               if x `gt` y then return (Ascending, k) else asc y (k+1)
{-# INLINE nextRun #-}

-- | Tests if a temporary buffer has a given size. If not, allocates a new
-- buffer and returns it instead of the old temporary buffer.
ensureCapacity :: (PrimMonad m, MVector v e)
               => Int -> v (PrimState m) e -> m (v (PrimState m) e)
ensureCapacity l tmpBuf
  | l <= length tmpBuf = return tmpBuf
  | otherwise          = new (2*l)
{-# INLINE ensureCapacity #-}

-- | Copy the slice [i,i+len) from vec to tmpBuf. If tmpBuf is not large enough,
-- a new buffer is allocated and used. Returns the buffer.
cloneSlice :: (PrimMonad m, MVector v e)
           => Int -- ^ i
           -> Int -- ^ len
           -> v (PrimState m) e -- ^ vec
           -> v (PrimState m) e -- ^ tmpBuf
           -> m (v (PrimState m) e)
cloneSlice i len vec tmpBuf = do
  tmpBuf' <- ensureCapacity len tmpBuf
  unsafeCopy (unsafeSlice 0 len tmpBuf') (unsafeSlice i len vec)
  return tmpBuf'
{-# INLINE cloneSlice #-}

-- | Number of consecutive times merge chooses the element from the same run
-- before galloping mode is activated.
minGallop :: Int
minGallop = 7
{-# INLINE minGallop #-}

-- | Merge the adjacent sorted slices [l,m) and [m,u) in vec. This is done by
-- copying the slice [l,m) to a temporary buffer. Returns the (enlarged)
-- temporary buffer.
mergeLo :: (PrimMonad m, MVector v e)
        => Comparison e
        -> v (PrimState m) e -- ^ vec
        -> Int -- ^ l
        -> Int -- ^ m
        -> Int -- ^ u
        -> v (PrimState m) e -- ^ tmpBuf
        -> m (v (PrimState m) e)
mergeLo cmp vec l m u tempBuf' = do
  tmpBuf <- cloneSlice l tmpBufLen vec tempBuf'
  vi <- unsafeRead tmpBuf 0
  vj <- unsafeRead vec m
  iter tmpBuf 0 m l vi vj minGallop minGallop
  return tmpBuf
 where
 gt  a b = cmp a b == GT
 gte a b = cmp a b /= LT
 tmpBufLen = m - l
 iter _ i _ _ _ _ _ _ | i >= tmpBufLen = return ()
 iter tmpBuf i j k _ _ _ _ | j >= u = do
   let from = unsafeSlice i (tmpBufLen-i) tmpBuf
       to   = unsafeSlice k (tmpBufLen-i) vec
   unsafeCopy to from
 iter tmpBuf i j k _ vj 0 _ = do
   i' <- gallopingSearchLeftPBounds (`gt` vj) tmpBuf i tmpBufLen
   let gallopLen = i' - i
       from = unsafeSlice i gallopLen tmpBuf
       to   = unsafeSlice k gallopLen vec
   unsafeCopy to from
   vi' <- unsafeRead tmpBuf i'
   iter tmpBuf i' j (k+gallopLen) vi' vj minGallop minGallop
 iter tmpBuf i j k vi _ _ 0 = do
   j' <- gallopingSearchLeftPBounds (`gte` vi) vec j u
   let gallopLen = j' - j
       from = slice j gallopLen vec
       to   = slice k gallopLen vec
   unsafeMove to from
   vj' <- unsafeRead vec j'
   iter tmpBuf i j' (k+gallopLen) vi vj' minGallop minGallop
 iter tmpBuf i j k vi vj ga gb
   | vj `gte` vi = do unsafeWrite vec k vi
                      vi' <- unsafeRead tmpBuf (i+1)
                      iter tmpBuf (i+1) j (k+1) vi' vj (ga-1) minGallop
   | otherwise   = do unsafeWrite vec k vj
                      vj' <- unsafeRead vec (j+1)
                      iter tmpBuf i (j+1) (k+1) vi vj' minGallop (gb-1)
{-# INLINE mergeLo #-}

-- | Merge the adjacent sorted slices [l,m) and [m,u) in vec. This is done by
-- copying the slice [j,k) to a temporary buffer. Returns the (enlarged)
-- temporary buffer.
mergeHi :: (PrimMonad m, MVector v e)
        => Comparison e
        -> v (PrimState m) e -- ^ vec
        -> Int -- ^ l
        -> Int -- ^ m
        -> Int -- ^ u
        -> v (PrimState m) e -- ^ tmpBuf
        -> m (v (PrimState m) e)
mergeHi cmp vec l m u tmpBuf' = do
  tmpBuf <- cloneSlice m tmpBufLen vec tmpBuf'
  vi <- unsafeRead vec (m-1)
  vj <- unsafeRead tmpBuf (tmpBufLen-1)
  iter tmpBuf (m-1) (tmpBufLen-1) (u-1) vi vj minGallop minGallop
  return tmpBuf
 where
 gt  a b = cmp a b == GT
 gte a b = cmp a b /= LT
 tmpBufLen = u - m
 iter _ _ j _ _ _ _ _ | j < 0 = return ()
 iter tmpBuf i j _ _ _ _ _ | i < l = do
   let from = unsafeSlice 0 (j+1) tmpBuf
       to   = unsafeSlice l (j+1) vec
   unsafeCopy to from
 iter tmpBuf i j k _ vj 0 _ = do
   i' <- gallopingSearchRightPBounds (`gt` vj) vec l i
   let gallopLen = i - i'
       from = slice (i'+1) gallopLen vec
       to   = slice (k-gallopLen+1) gallopLen vec
   unsafeMove to from
   vi' <- unsafeRead vec i'
   iter tmpBuf i' j (k-gallopLen) vi' vj minGallop minGallop
 iter tmpBuf i j k vi _ _ 0 = do
   j' <- gallopingSearchRightPBounds (`gte` vi) tmpBuf 0 j
   let gallopLen = j - j'
       from = slice (j'+1) gallopLen tmpBuf
       to   = slice (k-gallopLen+1) gallopLen vec
   unsafeCopy to from
   vj' <- unsafeRead tmpBuf j'
   iter tmpBuf i j' (k-gallopLen) vi vj' minGallop minGallop
 iter tmpBuf i j k vi vj ga gb
   | vi `gt` vj = do unsafeWrite vec k vi
                     vi' <- unsafeRead vec (i-1)
                     iter tmpBuf (i-1) j (k-1) vi' vj (ga-1) minGallop
   | otherwise  = do unsafeWrite vec k vj
                     vj' <- unsafeRead tmpBuf (j-1)
                     iter tmpBuf i (j-1) (k-1) vi vj' minGallop (gb-1)
{-# INLINE mergeHi #-}

-- | Merge the adjacent sorted slices A=[l,m) and B=[m,u) in vec. This begins
-- with galloping searches to find the index of vec[m] in A and the index of
-- vec[m-1] in B to reduce the sizes of A and B. Then it uses `mergeHi` or
-- `mergeLo` depending on whether A or B is larger. Returns the (enlarged)
-- temporary buffer.
merge :: (PrimMonad m, MVector v e)
      => Comparison e
      -> v (PrimState m) e -- ^ vec
      -> Int -- ^ l
      -> Int -- ^ m
      -> Int -- ^ u
      -> v (PrimState m) e -- ^ tmpBuf
      -> m (v (PrimState m) e)
merge cmp vec l m u tmpBuf = do
  vm <- unsafeRead vec m
  l' <- gallopingSearchLeftPBounds (`gt` vm) vec l m
  if l' >= m
    then return tmpBuf
    else do
      vn <- unsafeRead vec (m-1)
      u' <- gallopingSearchRightPBounds (`gte` vn) vec m u
      if u' <= m
        then return tmpBuf
        else (if (m-l') <= (u'-m) then mergeLo else mergeHi) cmp vec l' m u' tmpBuf
 where
 gt  a b = cmp a b == GT
 gte a b = cmp a b /= LT
{-# INLINE merge #-}
