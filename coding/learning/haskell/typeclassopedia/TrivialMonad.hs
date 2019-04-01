-- http://blog.sigfpe.com/2007/04/trivial-monad.html

data W a = W a deriving Show

returnW :: a -> W a
returnW x = W x

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

foo :: Int -> W Int
foo x = W (x+1)

-- c = bind foo (foo 1)
-- d = bind foo (bind foo (foo 1))

-- (1) define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y).
-- Obviously that definition won't do - the left hand side has a W y pattern so 
-- it's actually unwrapping. Rewrite this function so that the only unwrapping 
-- that happens is carried out by bind

g' :: Int -> W Int -> W Int
g' x (W y) = W (x+y)

g :: Int -> W Int -> W Int
g x = bind (\y -> returnW $ x + y)

-- (2) define a function h :: W Int -> W Int -> W Int so that 
-- h (W x) (W y) = W (x+y). Again, no unwrapping.

h' :: W Int -> W Int -> W Int
h' (W x) (W y) = W (x+y)

h :: W Int -> W Int -> W Int
h f0 f1 = bind (\x -> bind (\y -> returnW $ x+y) f1) f0

-- Exercise 3: Prove the three monad laws for W. This should be almost trivial.
-- Left identity: returnW a >>= f ≡ f a
-- Right identity:	m >>= return ≡ m
-- Associativity: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- bind :: (a -> W b) -> (W a -> W b)
-- bind f (W x) = f x

-- Left Identity
--   returnW a >>= f    ≡ f a
--   bind (returnW a) f ≡ f a
--   bind (W a) f       ≡ f a
--   f x                ≡ f a
--   OK: equal up to variable renaming

-- Right identity: 
--   m >>= return   ≡ m
--   bind m return  ≡ m
--   f return       ≡ m

-- Associativity: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- Exercise 4: We can't completely unwrap things using the monad API. But we 
-- can unwrap one layer from things that are wrapped twice. So here's a nice 
-- puzzle: define a function join :: W (W a) -> W a using the Monad API and no
-- explicit unwrapping.

join :: W (W a) -> W a
join = bind (bind returnW)
--join ww = bind (\w -> bind (\a -> returnW a) w) ww