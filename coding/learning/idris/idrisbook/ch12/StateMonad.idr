data State : (st : Type) -> Type -> Type where
  Get : State st st
  Put : st -> State st ()

  Pure : t -> State st t
  Bind : State st a -> (a -> State st b) -> State st b

(>>=) : State st a -> (a -> State st b) -> State st b
(>>=) = Bind

get : State st st
get = Get

put : st -> State st ()
put = Put

pure : t -> State st t
pure = Pure

mutual
  Functor (State st) where
    map func x = do
      a <- x
      pure (func a)

  Applicative (State st) where
    pure = Pure
    (<*>) f a = do
      f' <- f
      a' <- a
      pure (f' a')

  Monad (State st) where
    (>>=) = Bind
    
runState : State st a -> st -> (a, st)
runState Get st = (st, st)
runState (Put st) st0 = ((), st)
runState (Pure a) st = (a, st)
runState (Bind stAct stCont) st0
  = let (a, st') = runState stAct st0 in
        runState (stCont a) st'
        
addIfPositive : Integer -> State Integer Bool
addIfPositive x = do
  when (x > 0) $ do
    current <- get
    put (current + x)
  pure (x > 0)

addPositives : List Integer -> State Integer Nat
addPositives xs = do 
  added <- traverse addIfPositive xs
  pure (length (filter id added))