module SugarScape.Core.Utils 
  ( ifThenElse
  , ifThenElseM
  , orM
  , andM

  , untilM
  
  , uncurry3
  , uncurry4
  , uncurry5
  , uncurry6

  , flipBoolAtIdx

  , removeElemByIdx
  , findFirstDiffIdx
  , findMinWithIdx
  , hammingDistances
  ) where

import Control.Monad
import Data.List
import Data.Maybe

import Data.List.Split

-------------------------------------------------------------------------------
-- MONADIC UTILITIES
-------------------------------------------------------------------------------
ifThenElse :: Monad m 
           => Bool 
           -> m a 
           -> m a 
           -> m a
ifThenElse p trueAction falseAction 
  = if p then trueAction else falseAction

ifThenElseM :: Monad m 
            => m Bool 
            -> m a 
            -> m a 
            -> m a
ifThenElseM test trueAction falseAction 
  = test >>= \t -> if t then trueAction else falseAction

orM :: Monad m
    => m Bool
    -> m Bool
    -> m Bool
orM = liftM2 (||) 

andM :: Monad m
     => m Bool
     -> m Bool
     -> m Bool
andM = liftM2 (&&) 

untilM :: Monad m
       => m Bool
       -> m a
       -> m a
untilM p f = do
  ret <- p
  a   <- f
  if ret 
    then return a
    else untilM p f

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fun (a, b, c, d, e, f) = fun a b c d e f

removeElemByIdx :: Int -> [a] -> [a]
removeElemByIdx idx xs = pre ++ tail re
  where
    (pre, re) = splitAt idx xs

findMinWithIdx :: (Ord a) => [a] -> (a, Int)
findMinWithIdx as = (minA, minAIdx)
  where
    minA = minimum as
    minAIdx = fromJust $ elemIndex minA as

findFirstDiffIdx :: (Eq a) => [a] -> [a] -> Int
findFirstDiffIdx as bs = firstNotEqualIdx
  where
    notEquals = zipWith (/=) as bs
    firstNotEqualIdx = fromJust $ elemIndex True notEquals

flipBoolAtIdx :: Int -> [Bool] -> [Bool]
flipBoolAtIdx idx bs = front ++ (flippedElem : backNoElem)
  where
    (front, back) = splitAt idx bs  -- NOTE: back includes the element with the index
    elemAtIdx     = bs !! idx
    flippedElem   = not elemAtIdx
    backNoElem    = tail back

hammingDistances :: [Bool] -> [Bool] -> [Int]
hammingDistances i d = map (`hammingDistanceAux` d) isubs
  where
    dLen  = length d
    isubs = Data.List.Split.divvy dLen 1 i

    -- NOTE: both must have the same length
    hammingDistanceAux :: [Bool] -> [Bool] -> Int
    hammingDistanceAux as bs = length $ filter (==False) equals
      where
        equals = zipWith (==) as bs
