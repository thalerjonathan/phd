module Utils 
  ( ifThenElse
  , ifThenElseM
  ) where

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