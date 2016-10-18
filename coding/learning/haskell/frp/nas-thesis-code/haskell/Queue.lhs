> module Queue where

> import Data.Sequence
> import Control.Arrow
> import Control.Monad

> import Prelude hiding (length, reverse)

> type Queue a = Seq a

> emptyQueue       :: Queue a
> emptyQueue       =  empty

> newQueue         :: [a] -> Queue a
> newQueue         =  fromList

> singletonQueue   :: a -> Queue a
> singletonQueue   =  singleton 

> enQueue          :: a -> Queue a -> Queue a
> enQueue          =  (<|)

> enQueues         :: [a] -> Queue a -> Queue a
> enQueues as q    =  foldl (flip enQueue) q as


> deQueue          :: Queue a -> Maybe (Queue a, a)
> deQueue q        =  case viewr q of
>                       EmptyR   -> Nothing
>                       (q :> a) -> Just (q, a)

> rollQueue        :: a -> Queue a -> (Queue a, a)
> rollQueue a q    =  case viewr q of
>                       EmptyR   -> (emptyQueue, a)
>                       (q :> b) -> (a <| q, b) 

> deQueueIf        :: (a -> Bool) -> Queue a -> Maybe (Queue a, a)
> deQueueIf p q    =  do (q', a) <- deQueue q
>                        guard (p a)
>                        return (q', a) 


-- The first element out is the head of the result list

> deQueueWhile     :: (a -> Bool) -> Queue a -> (Queue a, [a])
> deQueueWhile p q =  case deQueueIf p q of
>                       Nothing       -> (q, [])
>                       Just (q', a)  -> second (a:) (deQueueWhile p q')

> deQueueWhileLast :: (a -> Bool) -> Queue a -> Maybe (Queue a , a)
> deQueueWhileLast p q = case deQueueWhile p q of
>                          (_  , []) -> Nothing
>                          (q' , as) -> Just (q' , last as)

> qreverse :: Queue a -> Queue a
> qreverse = reverse

> qlength :: Queue a -> Int
> qlength =  length

-------------------------------------------------------------------------
