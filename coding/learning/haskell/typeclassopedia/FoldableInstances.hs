{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Monoid (..), id, (.))
import Data.Monoid

-- instance Main.Foldable [] where
--   foldMap :: Monoid m => (a -> m) -> [a] -> m
--   foldMap g = mconcat . map g

class Foldable t where
  fold :: Monoid m => t m -> m
  -- 1. Implement fold in terms of foldMap.
  fold = foldMap id

  -- 2. What would you need in order to implement foldMap in terms of fold?
  -- ANSWER: some way of mapping the first argument function over the foldable
  -- structure t, this can be done, see exercise 3
  foldMap :: Monoid m => (a -> m) -> t a -> m

  -- 3. Implement foldMap in terms of foldr.
  foldMap g = foldr (\a b -> mappend (g a) b) mempty

  -- 4. Implement foldr in terms of foldMap (hint: use the Endo monoid).
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f b0 t = ef b0
    where
      -- use foldMap to build up a composition of Endo monoids which results
      -- in an Endo Monoid of type Endo b. This can then be used with appEndo
      -- to compute the result of foldr of type b using b0 as initial b
      em = foldMap (\a -> Endo (f a)) t
      -- unpack the endo function
      ef = appEndo em

-- 5. What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap,
-- etc.? What do they do?
-- :t (foldMap . foldMap) :: (Foldable t1, Foldable t2) => (a -> m) -> t1 (t2 m) -> m
-- :t (foldMap . foldMap . foldMap) :: (Foldable t1, Foldable t2, Foldable t3) => (a -> m) -> t1 (t2 (t3 m)) -> m
-- stacking up foldables within foldables