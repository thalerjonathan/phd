import Data.Foldable

-- 1. Implement traverse_ in terms of sequenceA_ and vice versa. One of these 
-- will need an extra constraint. What is it?

-- NOTE from Typeclassopedia: "applies the given function to each element in a 
-- foldable container and sequences the effects (but discards the results)."
-- GHCi: traverseSeq_ print [1..4]
traverseSeq_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
traverseSeq_ g t = sequenceA_ tfb
  where
    tfb = map g $ toList t

-- NOTE from Typeclassopedia: "sequenceA_ takes a container full of computations
-- and runs them in sequence, discarding the results (that is, they are used 
-- only for their effects). Since the results are discarded, the container only
-- needs to be Foldable. (Compare with 
-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a), 
-- which requires a stronger Traversable constraint in order to be able to
--  reconstruct a container of results having the same shape as the original 
-- container.)"
sequenceATra_ :: (Applicative f, Foldable t) => t (f a) -> f ()
sequenceATra_ = traverse_ runCompDiscRes
  where
    -- this function runs the computation and discards the result 
    runCompDiscRes :: Applicative f => f a -> f ()
    runCompDiscRes fa = pure (const ()) <*> fa 
                        -- (*>) fa (pure ())