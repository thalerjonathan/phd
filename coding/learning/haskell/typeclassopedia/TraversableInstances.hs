{-# LANGUAGE InstanceSigs #-}

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ b0 Empty = b0
  foldr g b0 (Leaf a) = g a b0
  foldr g b0 (Node l a r) = bl
    where
      br = foldr g b0 r
      ba = g a br
      bl = foldr g ba l

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty    = pure Empty
  traverse g (Leaf a) = ft
    where
      fb = g a
      ft = pure Leaf <*> fb 
        -- Leaf <$> fb

  traverse g (Node l a r) = pure Node <*> fl <*> fb <*> fr
                          -- Node <$> fl <*> fb <*> fr
    where
      fl = traverse g l
      fb = g a
      fr = traverse g r