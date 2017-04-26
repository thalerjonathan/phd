{-# LANGUAGE
             GADTs,
             ScopedTypeVariables,
             TypeOperators
    #-}

module PrimitivesD where

import Time
import Decoupled
import CoreD

import Data.Maybe
import Control.Monad
import Control.Arrow hiding ((>>>), (&&&), (***))
import Queue
import Utilities
import ArrowCombinators hiding (identity, toFst, toSnd, fst, snd)

---------------------------------------------------------

-- utility constructor functions, hidden from FRP programmer

mkSFcau :: (Dt -> q -> Sample as -> (q , Sample bs)) -> (Sample as -> (q , Sample bs)) -> SF as bs Cau
mkSFcau f g = CPrim (first (CNode f) . g)

mkSFdec :: (Dt -> q -> ((Sample as -> q) , Sample bs)) -> (Sample as -> q) -> Sample bs -> SF as bs Dec
mkSFdec f g = DPrim (DNode f . g)

mkSFsource :: (Dt -> q -> (q , Sample bs)) -> q -> Sample bs -> SF as bs Dec
mkSFsource f q = mkSFdec ((result2 . first) const f) (const q)

mkSFtimeless :: (q -> Sample as -> (q , Sample bs)) -> q -> SF as bs Cau
mkSFtimeless f q = mkSFcau (const f) (f q)

mkSFstateless :: (Sample as -> Sample bs) -> SF as bs Cau
mkSFstateless f = mkSFtimeless (curry (second f)) ()

mkSFchangeless :: Sample bs -> SF as bs Dec
mkSFchangeless sb = mkSFsource (\ _ _ -> (() , sb)) () sb


-- utility synonyms

noEvent :: Sample (E a)
noEvent = Nothing

event :: a -> Sample (E a)
event = Just

---------------------------------------------------------

identity :: SF as as Cau
identity = ARouter SFId

sfFst :: SF (as , bs) as Cau
sfFst = ARouter Fst

sfSnd :: SF (as , bs) bs Cau
sfSnd = ARouter Snd

infixr 1 >>>
infixr 3 &&&

(>>>) :: Decoupled d1 => SF as bs d1 -> SF bs cs d2 -> SF as cs (d1 :|: d2)
(>>>) = Seq

(&&&) :: Decoupled d1 => SF as bs d1 -> SF as cs d2 -> SF as (bs , cs) (d1 :&: d2)
(&&&) = Fan

switch :: Decoupled d1 => SF as (bs , E e) d1 -> (e -> SF as bs d2) -> SF as bs (d1 :&: d2)
switch = Switch

freeze :: SF as bs d -> SF as (bs , C (SF as bs d)) d
freeze = Freeze

loop :: SF (as , cs) bs d -> SF bs cs Dec -> SF as bs d
loop = Loop

weaken :: SF as bs d -> SF as bs Cau
weaken = Weaken

---------------------------------------------------------

constantS :: a -> SF as (S a) Dec
constantS = mkSFchangeless

never :: SF as (E a) Dec
never = mkSFchangeless noEvent

now :: SF as (E ()) Dec
now = mkSFsource (\ _ _ -> (() , noEvent)) () (event ())

notYet :: SF (E a) (E a) Cau
notYet = mkSFcau (\ _ -> curry id) (const (() , noEvent))

filterE :: (a -> Bool) -> SF (E a) (E a) Cau
filterE p = mkSFstateless (maybeFilter p)

hold :: a -> SF (E a) (S a) Cau
hold = mkSFtimeless (\ q -> fork . fromMaybe q)

edge :: SF (S Bool) (E ()) Cau
edge = mkSFtimeless (\ q i -> (i , (if i && not q then event () else noEvent))) True

when :: (a -> Bool) -> SF (C a) (E a) Cau
when p = mkSFtimeless (\ q i -> (p i , (if p i && not q then event i else noEvent))) True


-- integral has a state consisting of (Total , Previous Sample)

type IntegralState = (Double , Double)

integrateRectangle :: Dt -> IntegralState -> ((Double -> IntegralState) , Double)
integrateRectangle dt (tot , x1) =  let tot' = tot + (dt * x1) 
                                    in ((\ x2 -> (tot' , x2)) , tot')

integrateTrapezium :: Dt -> IntegralState -> Double -> (IntegralState , Double)
integrateTrapezium dt (tot , x1) x2 =  let tot' = tot + (dt * (x1 + x2) / 2)
                                       in ((tot' , x2) , tot')

integralS :: SF (S Double) (C Double) Dec
integralS = mkSFdec integrateRectangle (\ x0 -> (0 , x0)) 0

integralC :: SF (C Double) (C Double) Cau
integralC = mkSFcau integrateTrapezium (\ x0 -> ((0 , x0) , 0))

--------------------------------------------------------------------

liftC :: (a -> b) -> SF (C a) (C b) Cau
liftC = mkSFstateless

liftS :: (a -> b) -> SF (S a) (S b) Cau
liftS = mkSFstateless

liftE :: (a -> b) -> SF (E a) (E b) Cau
liftE = mkSFstateless . fmap

liftC2 :: (a -> b -> z) -> SF (C a , C b) (C z) Cau
liftC2 = mkSFstateless . uncurry

liftS2 :: (a -> b -> z) -> SF (S a , S b) (S z) Cau
liftS2 = mkSFstateless . uncurry

merge :: (a -> z) -> (b -> z) -> (a -> b -> z) -> SF (E a , E b) (E z) Cau
merge fa fb fab = mkSFstateless (uncurry (maybeMerge fa fb fab))

join :: (a -> b -> z) -> SF (E a , E b) (E z) Cau
join = mkSFstateless . uncurry . liftM2

sampleWithC :: (a -> b -> z) -> SF (C a , E b) (E z) Cau
sampleWithC f  = mkSFstateless (uncurry (fmap . f))

sampleWithS :: (a -> b -> z) -> SF (S a , E b) (E z) Cau
sampleWithS f = mkSFstateless (uncurry (fmap . f))

--------------------------------------------------------------------

fromS :: SF (S a) (C a) Cau
fromS = mkSFstateless id

dfromS :: a -> SF (S a) (C a) Dec
dfromS = mkSFdec (\ _ q -> (id , q)) id

-------------------------------------------------------------------

type DelayQueue  a = Queue (ReleaseTime , a)
type DelayStateC a = (CurrentTime , Maybe a , DelayQueue a)
type DelayStateS a = (CurrentTime , a , DelayQueue a)
type DelayStateE a = (CurrentTime , DelayQueue a)

ready :: CurrentTime -> (ReleaseTime , a) -> Bool
ready ct (rt , _) = ct >= rt

delayC :: forall a. Time -> (Time -> a) -> SF (C a) (C a) Dec
delayC d f = mkSFdec delayAuxC (\ a0 -> (0 , Nothing , enQueueC 0 a0 emptyQueue)) (f 0)
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

         delayAuxTimeC :: CurrentTime -> Maybe a -> DelayQueue a -> ((a -> DelayStateC a) , a)
         delayAuxTimeC t ma1 q =  let (ma2 , q' , a2) = deQueueCstate t ma1 q
                                  in ((\ a -> (t , ma2 , enQueueC t a q')) , a2)

         delayAuxC :: Dt -> DelayStateC a -> ((a -> DelayStateC a) , a)
         delayAuxC dt (t1 , ma1 , q) = delayAuxTimeC (dt + t1) ma1 q


delayS :: forall a. Time -> a -> SF (S a) (S a) Dec
delayS d a0 = mkSFdec delayAuxS (\ a1 -> (0 , a0 , enQueueS 0 a1 emptyQueue)) a0
   where
         enQueueS :: CurrentTime -> a -> DelayQueue a -> DelayQueue a
         enQueueS t a = enQueue (t + d , a)

         deQueueS :: CurrentTime -> a -> DelayQueue a -> (DelayQueue a , a)
         deQueueS t a1 q =  case deQueueWhileLast (ready t) q of
                              Nothing               -> (q  , a1)
                              Just (q' , (_ , a2))  -> (q' , a2)

         delayAuxTimeS :: CurrentTime -> a -> DelayQueue a -> ((a -> DelayStateS a) , a)
         delayAuxTimeS t a1 q =  let (q' , a2) = deQueueS t a1 q
                                 in ((\ a -> (t , a2 , enQueueS t a q')) , a2)

         delayAuxS :: Dt -> DelayStateS a -> ((a -> DelayStateS a) , a)
         delayAuxS dt (t1 , a1 , q) = delayAuxTimeS (dt + t1) a1 q


delayE :: forall a. Time -> SF (E a) (E a) Dec
delayE d = mkSFdec delayAuxE (\ e0 -> (0 , enQueueE 0 e0 emptyQueue)) noEvent
   where
         enQueueE :: CurrentTime -> Maybe a -> DelayQueue a -> DelayQueue a
         enQueueE _ Nothing  = id
         enQueueE t (Just a) = enQueue (t + d , a)

         deQueueE :: CurrentTime -> DelayQueue a -> (DelayQueue a , Maybe a)
         deQueueE t q =  case deQueueIf (ready t) q of
                           Nothing              -> (q  , noEvent)
                           Just (q' , (_ , a))  -> (q' , event a)

         delayAuxTimeE :: CurrentTime -> DelayQueue a -> ((Maybe a -> DelayStateE a) , Maybe a)
         delayAuxTimeE t q =  let (q' , e2) = deQueueE t q
                              in ((\ e -> (t , enQueueE t e q')) , e2)

         delayAuxE :: Dt -> DelayStateE a -> ((Maybe a -> DelayStateE a) , Maybe a)
         delayAuxE dt (t1 , q) = delayAuxTimeE (dt + t1) q 

-------------------------------------------------------------------
