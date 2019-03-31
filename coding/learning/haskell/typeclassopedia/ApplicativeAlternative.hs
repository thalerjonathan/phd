-- 1. Implement pure and (<*>) in terms of unit and (**), and vice versa.

import Prelude hiding ((**))

class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)

-- pure' :: Monoidal f => a -> f a
-- pure' a = fmap (const a) unit

-- (<*>) :: Monoidal f => f (a -> b) -> f a -> f b
-- (<*>) g a = fmap (uncurry $) ((**) g a)

-- unit' = pure ()
-- fa (**) fb = pure (,) <*> fa <*> fb

-- 2. Are there any Applicative instances for which there are also functions 
-- f () -> () and f (a,b) -> (f a, f b), satisfying some "reasonable" laws?
instance Monoidal Maybe where
  unit = Just ()
  (**) Nothing Nothing  = Nothing 
  (**) (Just _) Nothing = Nothing
  (**) Nothing (Just _) = Nothing
  (**) (Just a) (Just b) = Just (a, b)

-- 3. (Tricky) Prove that given your implementations from the first exercise, 
-- the usual Applicative laws and the Monoidal laws stated above are equivalent.