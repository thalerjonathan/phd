module SugarScape.Utils 
  ( ifThenElse
  , ifThenElseM
  , orM
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

orM :: Monad m
    => m Bool
    -> m Bool
    -> m Bool
orM m1 m2 = do
  b1 <- m1
  b2 <- m2
  return $ b1 || b2