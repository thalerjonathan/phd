{-# LANGUAGE OverloadedStrings #-}
-- Exercises for https://wiki.haskell.org/Typeclassopedia

-- Implement Functor instances for Either e and ((->) e).

-- NOTE BY THJO: we need parantheses arount Either with a type-parameter but I don't understand why
-- Maybe also doesn't need a type-parameter so I don't quite understand why it is necessary here
{-- duplicate instance declaration
instance Functor (Either e) where
  fmap g (Left a) = Left (g a)
  fmap g (Right a) = Right (g a)
--}

{-- from Prelude
NOTE: the meaning of a functor on a function is a function-composition (in haskell using .)
instance Functor ((->) r) where
    fmap = (.)
--}
---------------------------------------------------------------------
-- Implement Functor instances for ((,) e) and for Pair, defined as
data Pair a = Pair a a

instance Functor Pair where
  fmap = pairMap

pairMap :: (a -> b) -> Pair a -> Pair b
pairMap g (Pair x y) = Pair (g x) (g y) 

leftOfPair :: Pair a -> a
leftOfPair (Pair a b) = a

rightOfPair :: Pair a -> a
rightOfPair (Pair a b) = b

pairToTuple :: Pair a -> (a, a)
pairToTuple (Pair a b) = (a, b)

{-- from Prelude
NOTE: the meaning of this is to apply the function to the snd of a tuple
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)
--}
---------------------------------------------------------------------


-- Implement a Functor instance for the type ITree, defined as
-- NOTE a leaf holds a value which is actual a function taking an int and return a value of type a
-- the tree itself is a recursive data-structure
data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf x) = Leaf (g . x)
  -- NOTE: apply a function to a function by function composition , could also write Leaf (fmap g h) because of ((->), e) Functor does the exact same thing: apply a function to a function (A functor on functions)
  fmap g (Node xs) = Node (map (fmap g) xs)
  -- NOTE: we must apply the function g to all of the elements of the list xs (which are of type ITree which means they are either Node or Leaf). Because we have a list and must apply a function to it the map-function is used. To apply g to all elements we use (fmap g) because fmap will then do the correct pattern-matching and apply the function g correct to Leaf or Node
  
constructITree :: ITree Int
constructITree = Node [ Leaf (\x -> x + 1), Node [ Leaf (\x -> x + x), Leaf (\x -> x * 2), Node [  Leaf (\x -> x * x) ] ] ]
---------------------------------------------------------------------


{-- is already implemented in Prelude. This is my own implementation
instance Applicative Maybe where
  pure x = Just x -- could ommit x and write pure = Just
  Nothing <*> _ = Nothing -- Having no function, then will result in Nothing
  _ <*> Nothing = Nothing -- Having no value will result in no value
  Just f <*> Just x = Just (f x) -- could also write fmap f x
  -- NOTE: if implementing like Prelude: Just f <*> x = fmap f x one can ommit the case when the value is Nothing as it is implicitly covered by fmap of Maybe
--}

---------------------------------------------------------------------
---------------------------------------------------------------------
-- THE TRIVIAL MONAD
-- EXERCISES FROM http://blog.sigfpe.com/2007/04/trivial-monad.html

data W a = W a deriving Show

instance Functor W where
  fmap f (W x) = W (f x)

-- NOTE: need to define an Applicative instance for W as well when defining a Monad for W
instance Applicative W where
  pure x = W x
  W f <*> W v = W (f v)

instance Monad W where
  return x = W x
  W x >>= f = f x


f :: Int -> W Int
f x = W (x+1)

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

-- (1) define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y). Obviously that definition won't do - the left hand side has a W y pattern so it's actually unwrapping. Rewrite this function so that the only unwrapping that happens is carried out by bind.
g :: Int -> W Int -> W Int
g x w = bind (\y -> return (x+y)) w 
g' x w = w >>= (\y -> return (x+y)) -- NOTE: written using real 'bind' operator from Monad 

-- (2) define a function h :: W Int -> W Int -> W Int so that h (W x) (W y) = W (x+y). Again, no unwrapping.
h :: W Int -> W Int -> W Int
h w1 w2 = bind (\x -> bind (\y -> return (x+y)) w2) w1 
h' w1 w2 = w1 >>= (\x -> w2 >>= (\y -> return (x+y)))

-- Exercise 3: Prove the three monad laws for W. This should be almost trivial.
{--
Left identity:	
return a >>= f ≡ f a
Right identity:	
m >>= return ≡ m
Associativity:	
(m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
--}

-- Exercise 4: We can't completely unwrap things using the monad API. But we can unwrap one layer from things that are wrapped twice. So here's a nice puzzle: define a function join :: W (W a) -> W a using the Monad API and no explicit unwrapping.

join :: W (W a) -> W a
join w = w >>= (\innerW -> innerW)
---------------------------------------------------------------------
---------------------------------------------------------------------

---------------------------------------------------------------------
-- Exercises for Chapter 5 Monad in https://wiki.haskell.org/Typeclassopedia

-- 1. Implement a Monad instance for the list constructor, []. Follow the types!
{--
instance Monad [] where
  xs >>= f             = [y | x <- xs, y <- f x]
  -- NOTE: this implementation is taken from Prelude. 
--}

-- 2. Implement a Monad instance for ((->) e).
{--
instance Monad ((->) e) where
  f >>= k = \r -> k (f r) r
  -- NOTE: this implementation is taken from Prelude - I don't understand it
--}

-- 3. Implement Functor and Monad instances for Free f, defined as below. You may assume that f has a Functor instance. This is known as the free monad built from the functor f.
data Free f a = Var a
              | FNode (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap g (Var x) = Var (g x)
  fmap g (FNode x) = FNode (fmap (fmap g) x)
 
instance (Functor f) => Applicative (Free f) where
  pure = Var
  (Var f) <*> x = fmap f x
  (FNode f) <*> x = FNode (fmap (<*> x) f)
  
instance (Functor f) => Monad (Free f) where
  return = Var
  (Var x) >>= f = f x
--  (FNode x) >>= f = join (fmap f x)

-- 1. Implement (>>=) in terms of fmap (or liftM) and join.
-- m >>= f = join (fmap f a)

-- 2. Now implement join and fmap (liftM) in terms of (>>=) and return.
{--
	join :: (m (m a)) -> m a
	join a = a >>= id 
	fmap :: (a -> b) -> f a -> f b 
	fmap f a = a >>= (return . f)
--}
