module Main where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

type FooState = Int
  
rngSeed :: Int
rngSeed = 42

initState :: FooState
initState = 100

main :: IO ()
main = do
  let g = mkStdGen rngSeed
  let dt = 1.0
 
  let readerM = runReaderT foo dt
  let stateM = runStateT readerM initState
  let (x, initState') = evalRand stateM g 

  print $ "foo returns: " ++ show x
  print $ "updated state = " ++ show initState'
  
-- TODO: can we have a generic monad with a ReaderT AND / OR a RandT 
--       somewhere in the stack?

-- TODO: can we run a 
--       RandomGen g => ReaderT Double (StateT FooState (Rand g)) a
-- inside a
--       RandomGen g => ReaderT Double (Rand g) a

foo :: RandomGen g => ReaderT Double (StateT FooState (Rand g)) FooState
foo = do
  s <- get
  -- r <- lift . lift $ getRandom ((0, 100) :: (Int, Int))
  let r = 10
  put (s + r)
  return s