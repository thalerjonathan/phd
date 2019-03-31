-- (Tricky) One might imagine a variant of the interchange law that says 
-- something about applying a pure function to an effectful argument. Using 
-- the above laws, prove that
-- pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- the applicative definitions
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- the applicative laws
-- identity
--  pure id <*> v = v
--
-- homomorphism
--  pure f <*> pure x = pure (f x)
--
-- interchange
--  u <*> pure y = pure ($ y) <*> u
--
-- composition
--  u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

-- START
-- pure f <*> x = pure (flip ($)) <*> x <*> pure f
-- TODO