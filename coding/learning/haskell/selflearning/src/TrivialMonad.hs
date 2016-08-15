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
