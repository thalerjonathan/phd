module SugarScape.Core.Utils 
  ( ifThenElse
  , ifThenElseM
  , orM
  , andM

  , flipBoolAtIdx
  
  , uncurry3
  , uncurry4
  , uncurry5
  , uncurry6

  , removeElemByIdx
  ) where

import Control.Monad

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

flipBoolAtIdx :: Int -> [Bool] -> [Bool]
flipBoolAtIdx idx bs = front ++ (flippedElem : backNoElem)
  where
    (front, back) = splitAt idx bs  -- NOTE: back includes the element with the index
    elemAtIdx     = bs !! idx
    flippedElem   = not elemAtIdx
    backNoElem    = tail back

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