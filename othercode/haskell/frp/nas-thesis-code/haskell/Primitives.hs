{-# LANGUAGE
             GADTs,
             ScopedTypeVariables
    #-}

module Primitives where

import Time
import Core

import Data.Maybe
import Control.Monad
import Control.Arrow hiding ((>>>), (&&&), (***))
import Queue
import Utilities
import ArrowCombinators hiding (identity, toFst, toSnd, fst, snd)

---------------------------------------------------------

-- utility constructor functions, hidden from FRP programmer

mkSF :: (Dt -> q -> Sample as -> (q , Sample bs)) -> (Sample as -> (q , Sample bs)) -> SF as bs
mkSF f g = Prim (first (Node f) . g)

mkSFsource :: (Dt -> q -> (q , Sample bs)) -> q -> Sample bs -> SF as bs
mkSFsource f q sb = mkSF (\ dt q' _ -> f dt q') (const (q , sb))

mkSFtimeless :: (q -> Sample as -> (q , Sample bs)) -> q -> SF as bs
mkSFtimeless f q = mkSF (const f) (f q)

mkSFstateless :: (Sample as -> Sample bs) -> SF as bs
mkSFstateless f = mkSFtimeless (\ _ sa -> (() , f sa)) ()

mkSFchangeless :: Sample bs -> SF as bs
mkSFchangeless sb = mkSFstateless (const sb)


-- utility synonyms

noEvent :: Sample (E a)
noEvent = Nothing

event :: a -> Sample (E a)
event = Just

---------------------------------------------------------

identity :: SF as as
identity = ARouter SFId

sfFst :: SF (as , bs) as
sfFst = ARouter Fst

sfSnd :: SF (as , bs) bs
sfSnd = ARouter Snd

infixr 1 >>>
infixr 3 &&&

(>>>) :: SF as bs -> SF bs cs -> SF as cs
(>>>) = Seq

(&&&) :: SF as bs -> SF as cs -> SF as (bs , cs)
(&&&) = Fan

switch :: SF as (bs , E e) -> (e -> SF as bs) -> SF as bs
switch = Switch

freeze :: SF as bs -> SF as (bs , C (SF as bs))
freeze = Freeze

---------------------------------------------------------

constantS :: a -> SF as (S a)
constantS = mkSFchangeless

never :: SF as (E a)
never = mkSFchangeless noEvent

now :: SF as (E ())
now = mkSFsource (\ _ _ -> (() , noEvent)) () (event ())

notYet :: SF (E a) (E a)
notYet = mkSF (\ _ -> curry id) (const (() , noEvent))

filterE :: (a -> Bool) -> SF (E a) (E a)
filterE p = mkSFstateless (maybeFilter p)

hold :: a -> SF (E a) (S a)
hold = mkSFtimeless (\ q -> fork . fromMaybe q)

edge :: SF (S Bool) (E ())
edge = mkSFtimeless (\ q i -> (i , (if i && not q then event () else noEvent))) True

when :: (a -> Bool) -> SF (C a) (E a)
when p = mkSFtimeless (\ q i -> (p i , (if p i && not q then event i else noEvent))) True



-- integral has a state consisting of (Total , Previous Sample)

type IntegralState = (Double , Double)

integrateRectangle :: Dt -> IntegralState -> Double -> (IntegralState , Double)
integrateRectangle dt (tot , x1) x2 =  let tot' = tot + (dt * x1)
                                       in ((tot' , x2) , tot')

integrateTrapezium :: Dt -> IntegralState -> Double -> (IntegralState , Double)
integrateTrapezium dt (tot , x1) x2 =  let tot' = tot + (dt * (x1 + x2) / 2)
                                       in ((tot' , x2) , tot')

integralS :: SF (S Double) (C Double)
integralS = mkSF integrateRectangle (\ x0 -> ((0 , x0) , 0))

integralC :: SF (C Double) (C Double)
integralC = mkSF integrateTrapezium (\ x0 -> ((0 , x0) , 0))


--------------------------------------------------------------------

liftC :: (a -> b) -> SF (C a) (C b)
liftC = mkSFstateless

liftS :: (a -> b) -> SF (S a) (S b)
liftS = mkSFstateless

liftE :: (a -> b) -> SF (E a) (E b)
liftE = mkSFstateless . fmap

liftC2 :: (a -> b -> z) -> SF (C a , C b) (C z)
liftC2 = mkSFstateless . uncurry

liftS2 :: (a -> b -> z) -> SF (S a , S b) (S z)
liftS2 = mkSFstateless . uncurry

merge :: (a -> z) -> (b -> z) -> (a -> b -> z) -> SF (E a , E b) (E z)
merge fa fb fab = mkSFstateless (uncurry (maybeMerge fa fb fab))

join :: (a -> b -> z) -> SF (E a , E b) (E z)
join = mkSFstateless . uncurry . liftM2

sampleWithC :: (a -> b -> z) -> SF (C a , E b) (E z)
sampleWithC f  = mkSFstateless (uncurry (fmap . f))

sampleWithS :: (a -> b -> z) -> SF (S a , E b) (E z)
sampleWithS f = mkSFstateless (uncurry (fmap . f))

--------------------------------------------------------------------

fromS :: SF (S a) (C a)
fromS = mkSFstateless id

dfromS :: a -> SF (S a) (C a)
dfromS = mkSFtimeless (flip (,))

-------------------------------------------------------------------

type DelayQueue  a = Queue (ReleaseTime , a)
type DelayStateC a = (CurrentTime , Maybe a , DelayQueue a)
type DelayStateS a = (CurrentTime , a , DelayQueue a)
type DelayStateE a = (CurrentTime , DelayQueue a)

ready :: CurrentTime -> (ReleaseTime , a) -> Bool
ready ct (rt , _) = ct >= rt

delayC :: forall a. Time -> (Time -> a) -> SF (C a) (C a)
delayC d f = mkSF delayAuxC (\ a0 -> ((0 , Nothing , enQueueC 0 a0 emptyQueue) , f 0))
  where
         -- The "Maybe A" tells us whether we are still in the delay period (Nothing) 
         -- or not (Just a, where a is the most recent output sample)
         
         enQueueC :: CurrentTime -> a -> DelayQueue a -> DelayQueue a
         enQueueC t a = enQueue (t + d , a)

         deQueueC :: CurrentTime -> DelayQueue a -> Maybe (DelayQueue a , (ReleaseTime , a))
         deQueueC t = deQueueWhileLast (ready t)

         deQueueCstate :: CurrentTime -> Maybe a -> DelayQueue a -> (Maybe a , DelayQueue a , a)
         deQueueCstate t st q =  case deQueueC t q of
                                   Just (q' , (_ , a2)) ->  (Just a2 , q' , a2)
                                   Nothing              ->  case st of
                                                              Nothing  -> (Nothing , q , f t)
                                                              Just a1  -> (Just a1 , q , a1)

         delayAuxTimeC :: CurrentTime -> Maybe a -> DelayQueue a -> a -> (DelayStateC a , a)
         delayAuxTimeC t ma1 q a =  let (ma2 , q' , a2) = deQueueCstate t ma1 q
                                    in ((t , ma2 , enQueueC t a q') , a2)

         delayAuxC :: Dt -> DelayStateC a -> a -> (DelayStateC a , a)
         delayAuxC dt (t1 , ma1 , q) = delayAuxTimeC (dt + t1) ma1 q


delayS :: forall a. Time -> a -> SF (S a) (S a)
delayS d a0 = mkSF delayAuxS (\ a1 -> ((0 , a0 , enQueueS 0 a1 emptyQueue) , a0))
   where
         enQueueS :: CurrentTime -> a -> DelayQueue a -> DelayQueue a
         enQueueS t a = enQueue (t + d , a)

         deQueueS :: CurrentTime -> a -> DelayQueue a -> (DelayQueue a , a)
         deQueueS t a1 q =  case deQueueWhileLast (ready t) q of
                              Nothing               -> (q  , a1)
                              Just (q' , (_ , a2))  -> (q' , a2)

         delayAuxTimeS :: CurrentTime -> a -> DelayQueue a -> a -> (DelayStateS a , a)
         delayAuxTimeS t a1 q a =  let (q' , a2) = deQueueS t a1 q
                                   in ((t , a2 , enQueueS t a q') , a2)

         delayAuxS :: Dt -> DelayStateS a -> a -> (DelayStateS a , a)
         delayAuxS dt (t1 , a1 , q) = delayAuxTimeS (dt + t1) a1 q


delayE :: forall a. Time -> SF (E a) (E a)
delayE d = mkSF delayAuxE (\ e0 -> ((0 , enQueueE 0 e0 emptyQueue) , noEvent))
   where
         enQueueE :: CurrentTime -> Maybe a -> DelayQueue a -> DelayQueue a
         enQueueE _ Nothing  = id
         enQueueE t (Just a) = enQueue (t + d , a)

         deQueueE :: CurrentTime -> DelayQueue a -> (DelayQueue a , Maybe a)
         deQueueE t q =  case deQueueIf (ready t) q of
                           Nothing              -> (q  , noEvent)
                           Just (q' , (_ , a))  -> (q' , event a)

         delayAuxTimeE :: CurrentTime -> DelayQueue a -> Maybe a -> (DelayStateE a , Maybe a)
         delayAuxTimeE t q e =  let (q' , e2) = deQueueE t q
                                in ((t , enQueueE t e q') , e2)

         delayAuxE :: Dt -> DelayStateE a -> Maybe a -> (DelayStateE a , Maybe a)
         delayAuxE dt (t1 , q) = delayAuxTimeE (dt + t1) q 

-------------------------------------------------------------------
