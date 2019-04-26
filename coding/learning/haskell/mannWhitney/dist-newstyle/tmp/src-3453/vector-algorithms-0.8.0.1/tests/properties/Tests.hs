{-# LANGUAGE RankNTypes, TypeOperators, FlexibleContexts #-}

module Main (main) where

import Properties

import Util

import Test.QuickCheck

import Control.Monad
import Control.Monad.ST

import Data.Int
import Data.Word

import qualified Data.ByteString as B

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

import qualified Data.Vector.Algorithms.Insertion    as INS
import qualified Data.Vector.Algorithms.Intro        as INT
import qualified Data.Vector.Algorithms.Merge        as M
import qualified Data.Vector.Algorithms.Radix        as R
import qualified Data.Vector.Algorithms.Heap         as H
import qualified Data.Vector.Algorithms.Optimal      as O
import qualified Data.Vector.Algorithms.AmericanFlag as AF
import qualified Data.Vector.Algorithms.Tim          as T

import qualified Data.Vector.Algorithms.Search       as SR

type Algo      e r = forall s mv. MVector mv e => mv s e -> ST s r
type SizeAlgo  e r = forall s mv. MVector mv e => mv s e -> Int -> ST s r
type BoundAlgo e r = forall s mv. MVector mv e => mv s e -> Int -> Int -> ST s r

newtype WrappedAlgo      e r = WrapAlgo      { unWrapAlgo      :: Algo      e r }
newtype WrappedSizeAlgo  e r = WrapSizeAlgo  { unWrapSizeAlgo  :: SizeAlgo  e r }
newtype WrappedBoundAlgo e r = WrapBoundAlgo { unWrapBoundAlgo :: BoundAlgo e r }

args = stdArgs
       { maxSuccess = 1000
       , maxDiscardRatio = 2
       }

check_Int_sort = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_fullsort (unWrapAlgo algo))
 where
 algos :: [(String, WrappedAlgo Int ())]
 algos = [ ("introsort", WrapAlgo INT.sort)
         , ("insertion sort", WrapAlgo INS.sort)
         , ("merge sort", WrapAlgo M.sort)
         , ("heapsort", WrapAlgo H.sort)
         , ("timsort", WrapAlgo T.sort)
         ]

check_Int_partialsort = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_partialsort (unWrapSizeAlgo algo))
 where
 algos :: [(String, WrappedSizeAlgo Int ())]
 algos = [ ("intro-partialsort", WrapSizeAlgo INT.partialSort)
         , ("heap partialsort", WrapSizeAlgo H.partialSort)
         ]

check_Int_select = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_select (unWrapSizeAlgo algo))
 where
 algos :: [(String, WrappedSizeAlgo Int ())]
 algos = [ ("intro-select", WrapSizeAlgo INT.select)
         , ("heap select", WrapSizeAlgo H.select)
         ]

check_radix_sorts = do
  qc (label "radix Word8"       . prop_fullsort (R.sort :: Algo Word8  ()))
  qc (label "radix Word16"      . prop_fullsort (R.sort :: Algo Word16 ()))
  qc (label "radix Word32"      . prop_fullsort (R.sort :: Algo Word32 ()))
  qc (label "radix Word64"      . prop_fullsort (R.sort :: Algo Word64 ()))
  qc (label "radix Word"        . prop_fullsort (R.sort :: Algo Word   ()))
  qc (label "radix Int8"        . prop_fullsort (R.sort :: Algo Int8   ()))
  qc (label "radix Int16"       . prop_fullsort (R.sort :: Algo Int16  ()))
  qc (label "radix Int32"       . prop_fullsort (R.sort :: Algo Int32  ()))
  qc (label "radix Int64"       . prop_fullsort (R.sort :: Algo Int64  ()))
  qc (label "radix Int"         . prop_fullsort (R.sort :: Algo Int    ()))
  qc (label "radix (Int, Int)"  . prop_fullsort (R.sort :: Algo (Int, Int) ()))

  qc (label "flag Word8"       . prop_fullsort (AF.sort :: Algo Word8  ()))
  qc (label "flag Word16"      . prop_fullsort (AF.sort :: Algo Word16 ()))
  qc (label "flag Word32"      . prop_fullsort (AF.sort :: Algo Word32 ()))
  qc (label "flag Word64"      . prop_fullsort (AF.sort :: Algo Word64 ()))
  qc (label "flag Word"        . prop_fullsort (AF.sort :: Algo Word   ()))
  qc (label "flag Int8"        . prop_fullsort (AF.sort :: Algo Int8   ()))
  qc (label "flag Int16"       . prop_fullsort (AF.sort :: Algo Int16  ()))
  qc (label "flag Int32"       . prop_fullsort (AF.sort :: Algo Int32  ()))
  qc (label "flag Int64"       . prop_fullsort (AF.sort :: Algo Int64  ()))
  qc (label "flag Int"         . prop_fullsort (AF.sort :: Algo Int    ()))
  qc (label "flag ByteString"  . prop_fullsort (AF.sort :: Algo B.ByteString ()))
 where
 qc algo = quickCheckWith args algo

{-
check_schwartzian = do
  quickCheckWith args (prop_schwartzian i2w INS.sortBy)
 where
 i2w :: Int -> Word
 i2w = fromIntegral
-}

check_stable = do quickCheckWith args (label "merge sort" . prop_stable M.sortBy)
                  quickCheckWith args (label "radix sort" . prop_stable_radix R.sortBy)
                  quickCheckWith args (label "tim sort" . prop_stable T.sortBy)


check_optimal = do qc . label "size 2" $ prop_optimal 2 O.sort2ByOffset
                   qc . label "size 3" $ prop_optimal 3 O.sort3ByOffset
                   qc . label "size 4" $ prop_optimal 4 O.sort4ByOffset
 where
 qc = quickCheck

check_permutation = do
  qc $ label "introsort"    . prop_permutation (INT.sort :: Algo Int ())
  qc $ label "heapsort"     . prop_permutation (H.sort :: Algo Int ())

  qc $ label "mergesort"    . prop_permutation (M.sort :: Algo Int    ())
  qc $ label "timsort"      . prop_permutation (T.sort :: Algo Int    ())
  qc $ label "radix I8"     . prop_permutation (R.sort :: Algo Int8   ())
  qc $ label "radix I16"    . prop_permutation (R.sort :: Algo Int16  ())
  qc $ label "radix I32"    . prop_permutation (R.sort :: Algo Int32  ())
  qc $ label "radix I64"    . prop_permutation (R.sort :: Algo Int64  ())
  qc $ label "radix Int"    . prop_permutation (R.sort :: Algo Int    ())
  qc $ label "radix W8"     . prop_permutation (R.sort :: Algo Word8  ())
  qc $ label "radix W16"    . prop_permutation (R.sort :: Algo Word16 ())
  qc $ label "radix W32"    . prop_permutation (R.sort :: Algo Word32 ())
  qc $ label "radix W64"    . prop_permutation (R.sort :: Algo Word64 ())
  qc $ label "radix Word"   . prop_permutation (R.sort :: Algo Word   ())
  qc $ label "flag I8"      . prop_permutation (AF.sort :: Algo Int8   ())
  qc $ label "flag I16"     . prop_permutation (AF.sort :: Algo Int16  ())
  qc $ label "flag I32"     . prop_permutation (AF.sort :: Algo Int32  ())
  qc $ label "flag I64"     . prop_permutation (AF.sort :: Algo Int64  ())
  qc $ label "flag Int"     . prop_permutation (AF.sort :: Algo Int    ())
  qc $ label "flag W8"      . prop_permutation (AF.sort :: Algo Word8  ())
  qc $ label "flag W16"     . prop_permutation (AF.sort :: Algo Word16 ())
  qc $ label "flag W32"     . prop_permutation (AF.sort :: Algo Word32 ())
  qc $ label "flag W64"     . prop_permutation (AF.sort :: Algo Word64 ())
  qc $ label "flag Word"    . prop_permutation (AF.sort :: Algo Word   ())
  qc $ label "flag ByteString" . prop_permutation (AF.sort :: Algo B.ByteString ())
  qc $ label "intropartial" . prop_sized (\x -> const (prop_permutation x))
                                         (INT.partialSort :: SizeAlgo Int ())
  qc $ label "introselect"  . prop_sized (\x -> const (prop_permutation x))
                                         (INT.select :: SizeAlgo Int ())
  qc $ label "heappartial"  . prop_sized (\x -> const (prop_permutation x))
                                         (H.partialSort :: SizeAlgo Int ())
  qc $ label "heapselect"   . prop_sized (\x -> const (prop_permutation x))
                                         (H.select :: SizeAlgo Int ())

 where
 qc prop = quickCheckWith args prop

check_corners = do
  qc "introsort empty"    $ prop_empty       (INT.sort        :: Algo Int ())
  qc "intropartial empty" $ prop_sized_empty (INT.partialSort :: SizeAlgo Int ())
  qc "introselect empty"  $ prop_sized_empty (INT.select      :: SizeAlgo Int ())
  qc "heapsort empty"     $ prop_empty       (H.sort          :: Algo Int ())
  qc "heappartial empty"  $ prop_sized_empty (H.partialSort   :: SizeAlgo Int ())
  qc "heapselect empty"   $ prop_sized_empty (H.select        :: SizeAlgo Int ())
  qc "mergesort empty"    $ prop_empty       (M.sort          :: Algo Int ())
  qc "timsort empty"      $ prop_empty       (T.sort          :: Algo Int ())
  qc "radixsort empty"    $ prop_empty       (R.sort          :: Algo Int ())
  qc "flagsort empty"     $ prop_empty       (AF.sort         :: Algo Int ())
 where
 qc s prop = quickCheckWith (stdArgs { maxSuccess = 2 }) (label s prop)

type SAlgo e r = forall s mv. MVector mv e => mv s e -> e -> ST s r
type BoundSAlgo e r = forall s mv. MVector mv e => mv s e -> e -> Int -> Int -> ST s r

check_search_range = do
  qc $ (label "binarySearchL" .)
         . prop_search_inrange (SR.binarySearchLByBounds compare :: BoundSAlgo Int Int)
  qc $ (label "binarySearchL lo-bound" .)
         . prop_search_lowbound (SR.binarySearchL :: SAlgo Int Int)
  qc $ (label "binarySearch" .)
         . prop_search_inrange (SR.binarySearchByBounds compare :: BoundSAlgo Int Int)
  qc $ (label "binarySearchR" .)
         . prop_search_inrange (SR.binarySearchRByBounds compare :: BoundSAlgo Int Int)
  qc $ (label "binarySearchR hi-bound" .)
         . prop_search_upbound (SR.binarySearchR :: SAlgo Int Int)
 where
 qc prop = quickCheckWith args prop

main = do putStrLn "Int tests:"
          check_Int_sort
          check_Int_partialsort
          check_Int_select
          putStrLn "Radix sort tests:"
          check_radix_sorts
--          putStrLn "Schwartzian transform (Int -> Word):"
--          check_schwartzian
          putStrLn "Stability:"
          check_stable
          putStrLn "Optimals:"
          check_optimal
          putStrLn "Permutation:"
          check_permutation
          putStrLn "Search in range:"
          check_search_range
          putStrLn "Corner cases:"
          check_corners
