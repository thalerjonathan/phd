--------------------------------------------------------------------------------
-- 1. Implement (>>=) in terms of fmap (or liftM) and join.

bind :: Monad m => m a -> (a -> m b) -> m b
bind x f = join $ fmap f x 

--------------------------------------------------------------------------------
-- 2. Now implement join and fmap (liftM) in terms of (>>=) and return.
join :: Monad m => m (m a) -> m a
join mm = (>>=) mm id

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f mx = (>>=) mx (return . f) 