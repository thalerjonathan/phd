-- needed to hide (->) and (,)
{-# LANGUAGE NoImplicitPrelude #-}

-- not including Prelude implicitly because can hide (->) and (,) only using
-- LANGUAGE extention, thus we need to import the necessary bits manually
import Prelude (Show (..), Int, map, (.), ($))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

--------------------------------------------------------------------------------
-- 1. Implement Functor instances for Either e and ((->) e).
data Either e a = Left e | Right a deriving Show

-- Solution Either: the Functor instance for Either needs to take the type 
-- parameter e because Functor can only work on kinds * -> *. This also implies 
-- that we cannot fmap over both values e and a in Either: the natural choice is
-- to fmap over Right a because Left e indiceates failure and we shall leave
-- the failure value untouched 
instance Functor (Either e) where
   fmap _ (Left e)  = Left e
   fmap f (Right a) = Right $ f a

-- Solution ((->) e): the Functor instance needs to take the type parameter e
-- because the kind of Functor is * -> * but (->) has * -> * -> *. (->) is 
-- simply function application and the functor is function composition: the 
-- result of (->) e is an a (e -> a) and to go from e to b using (a -> b) we
-- simply use (.).
instance Functor ((->) e) where
  fmap = (.)
--  fmap f g = f . g -- more explicitly

--------------------------------------------------------------------------------
-- 2. Implement Functor instances for ((,) e) and for Pair, defined as
-- data Pair a = Pair a a
-- Explain their similarities and differences.

-- Same approach and problem with ((->) e) 
instance Functor ((,) e) where
  fmap f (e, a) = (e, f a)
-- fmap f t = (fst t, f $ snd t)

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- They represent both pairs but in the first case the first element is not
-- changed in the functor. It is simply not possible because Functor can only
-- map over a single type a and not a and e. (e,) can be seen as a value which
-- is tagged with a value e which does not change in case of fmap.
-- Pair a has two elements of the same type a and thus the Functor is able to
-- and indeed does map over both.

--------------------------------------------------------------------------------
-- 3. Implement a Functor instance for the type ITree, defined as
-- data ITree a = Leaf (Int -> a) 
--              | Node [ITree a]

data ITree a = Leaf (Int -> a) -- the leave has a function from an Index to a value
             | Node [ITree a]  -- the node has a list of ITrees

instance Functor ITree where
  -- Functor maps the function of the leaf to a composition 
  fmap f (Leaf g)  = Leaf (f . g)
  -- each element of the list of ITrees in the Node needs to be fmaped
  fmap f (Node ts) = Node ts'
    where
      ts' = map (fmap f) ts

--------------------------------------------------------------------------------
-- 4. Give an example of a type of kind * -> * which cannot be made an instance 
-- of Functor (without using undefined).

-- this should do the trick: Foo a has * -> * kind but a is a phantom type which
-- is actually nevery used in any of its data-constructors: we simply add
-- a single data-constructor with a MONOMORPHIC type. That is enough to 
-- make it impossible to implement a Functor instance for it because there
-- is no way we can get a b because we don't have a value of type a!
data NoFunctorDataType a = IntValue Int

--instance Functor NoFunctorDataType where
  -- impossible: cannot go from a to b because we don't have any a to produce
  -- a b using f. 
--  fmap f x = ?

--------------------------------------------------------------------------------
-- Is this statement true or false?
-- The composition of two Functors is also a Functor.
-- If false, give a counterexample; if true, prove it by exhibiting some 
-- appropriate Haskell code.

-- True! Simply looking at the type of 
-- (fmap . fmap) :: (a -> b) -> f1 (f2 a) -> f1 (f2 b) shold give enought proof.
-- A more concrete example using Maybe:
-- funcComp = (fmap . fmap) (+1)
-- funcComp (Just (Just 3))
--  = Just (Just 4)