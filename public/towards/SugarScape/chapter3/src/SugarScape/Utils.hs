module SugarScape.Utils 
  ( ifThenElse
  , ifThenElseM
  , orM
  , andM
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