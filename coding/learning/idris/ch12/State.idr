import Control.Monad.State

increase : Nat -> State Nat ()
increase inc = do
  curr <- get
  put (curr + inc)