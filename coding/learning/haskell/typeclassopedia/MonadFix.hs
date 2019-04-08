{-# LANGUAGE InstanceSigs #-}

class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a

-- my solution is wrong? seems to have the same effect...
instance MonadFix [] where
  mfix :: (a -> [a]) -> [a]
  mfix f = ma
    where
      ma = f (head ma)

-- from prelude
-- instance MonadFix [] where
--   mfix :: (a -> [a]) -> [a]
--   mfix f = case fix (f . head) of
--             []    -> []
--             (x:_) -> x : mfix (tail . f)
    
-- fix f = let x = f x in x