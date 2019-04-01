{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs      #-}

import Prelude (map, concat, const, (.), undefined)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

--------------------------------------------------------------------------------
-- 1. Implement a Monad instance for the list constructor, []. Follow the types!
instance Monad [] where
  return :: a -> [a]
  return x = [x]

  (>>=) :: [a] -> (a -> [b]) -> [b]
  (>>=) xs f = concat (map f xs)

--------------------------------------------------------------------------------
-- 2. Implement a Monad instance for ((->) e).
instance Monad ((->) e) where
  return :: a -> (e -> a)
  return = const
  --return a = (\e -> a)

  (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
  (>>=) f g = (\e -> g (f e) e)

--------------------------------------------------------------------------------
-- Implement Functor and Monad instances for Free f, defined as
-- data Free f a = Var a
--               | Node (f (Free f a))
-- You may assume that f has a Functor instance. This is known as the free monad
-- built from the functor f.

data Free f a = Var a
              | Node (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> (Free f a -> Free f b)
  fmap g (Var a)     = Var (g a)
  fmap g (Node free) = Node free'
    where
      free' = fmap (fmap g) free

instance Functor f => Monad (Free f) where
  return :: a -> Free f a
  return a = Var a

  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (>>=) (Var a) g     = g a
  (>>=) (Node free) g = Node free'
    where
      free' = fmap (>>= g) free
      --free'' = fmap (\free' -> (>>=) free' g) free
