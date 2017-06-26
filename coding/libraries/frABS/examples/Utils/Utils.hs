module Utils.Utils (
  	ifThenElseM
  ) where

-- NOTE: inspired by IfElse 
ifThenElseM :: Monad m => m Bool -> m () -> m () -> m ()
ifThenElseM test trueAction falseAction = test >>= \t -> if t then trueAction else falseAction