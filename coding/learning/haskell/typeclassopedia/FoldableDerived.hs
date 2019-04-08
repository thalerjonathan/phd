
import Data.Foldable

-- 1. Implement toList :: Foldable f => f a -> [a] in terms of either foldr 
-- or foldMap.
toListFoldr :: Foldable f => f a -> [a]
toListFoldr = foldr (:) []

toListFoldMap :: Foldable f => f a -> [a]
toListFoldMap f = fm []
  where
    fm = foldMap (:) f

-- 2. Show how one could implement the generic version of foldr in terms of 
-- toList, assuming we had only the list-specific
-- foldr :: (a -> b -> b) -> b -> [a] -> b.
foldrGen :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldrGen f b0 t = foldr f b0 as
  where
    as = toList t

-- 3. Pick some of the following functions to implement: concat, concatMap, and, 
-- or, any, all, sum, product, maximum(By), minimum(By), elem, notElem, and 
-- find. Figure out how they generalize to Foldable and come up with elegant 
-- implementations using fold or foldMap along with appropriate Monoid
-- instances.
-- TODO

-- use to test these functions
data Tree a = Node (Tree a) a (Tree a) | Leaf a

ts :: Tree Int
ts = Node (Node (Leaf 2) 3 (Leaf 4)) 5
          (Node (Leaf 6) 7 (Leaf 9))

instance Foldable Tree where
  -- NOTE: implement foldr :: (a -> a -> b) -> b -> t a -> b
  -- foldr :: (a -> a -> b) -> b -> Tree a -> b
  foldr f b (Leaf a)     = f a b
  foldr f b (Node l a r) = b'''
    where
      -- NOTE: apply RIGHT first, then elem of node, then LEFT because its a 
      -- RIGHT fold, so right comes first
      b'  = foldr f b r
      b'' = f a b'
      b''' = foldr f b'' l
