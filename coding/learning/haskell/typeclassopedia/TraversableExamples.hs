-- 1. Implement fmap and foldMap using only the Traversable methods. (Note that
-- the Traversable module provides these implementations as fmapDefault and 
-- foldMapDefault.)

-- NOTE: we need to add Traversable t here, otherwise this is not possible
fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f ta = traverse (pure f) ta

  -- f <$> ta would work but that is cheating :D

-- NOTE: we need to add Traversable t here, otherwise this is not possible
foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' g ta = undefined

-- 2. Implement Traversable instances for [], Maybe, ((,) e), and Either e.

-- 3. Explain why Set is Foldable but not Traversable.

-- 4. Show that Traversable functors compose: that is, implement an instance for
-- Traversable (Compose f g) given Traversable instances for f and g.