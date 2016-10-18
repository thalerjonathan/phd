{-# LANGUAGE
             GADTs,
             TypeOperators
    #-}

module LibraryD where

import Time
import Decoupled
import BouncingBall
import CoreD
import PrimitivesD

------------------------------------------------------------------

sfSwap :: SF (as , bs) (bs , as) Cau
sfSwap = sfSnd &&& sfFst

toFst :: SF as cs d -> SF (as , bs) cs d
toFst sf = sfFst >>> sf

toSnd :: SF bs cs d -> SF (as , bs) cs d
toSnd sf = sfSnd >>> sf

(***) :: Decoupled d1 => SF as cs d1 -> SF bs ds d2 -> SF (as , bs) (cs , ds) (d1 :&: d2)
sf1 *** sf2 = toFst sf1 &&& toSnd sf2

sfFirst :: Decoupled d => SF as bs d -> SF (as , cs) (bs , cs) Cau
sfFirst sf =  case drep sf of
                Cau  -> sf *** identity
                Dec  -> sf *** identity

sfSecond :: SF bs cs d -> SF (as , bs) (as , cs) Cau
sfSecond sf = identity *** sf

sfFork :: SF as (as , as) Cau
sfFork = identity &&& identity

forkFirst :: Decoupled d => SF as bs d -> SF as (bs , as) Cau
forkFirst sf =  case drep sf of
                    Cau  -> sf &&& identity
                    Dec  -> sf &&& identity

forkSecond :: SF as bs d -> SF as (as , bs) Cau
forkSecond sf = identity &&& sf

sfAssocR :: SF ((as , bs) , cs) (as , (bs , cs)) Cau
sfAssocR = toFst sfFst &&& sfFirst sfSnd

sfAssocL :: SF (as , (bs , cs)) ((as , bs) , cs) Cau
sfAssocL = sfSecond sfFst &&& toSnd sfSnd

-------------------------------------------------------------------

rswitch :: (Decoupled d1 , Decoupled d2) =>  SF as (bs , E e) d1 -> (e -> SF as (bs , E e) d2) -> SF as bs (d1 :&: d2)
rswitch sf f =  case drepf f of
                  Cau  -> switch sf (\ e -> rswitch (f e >>> sfSecond notYet) f)
                  Dec  -> switch sf (\ e -> rswitch (f e >>> sfSecond notYet) f)

switchWhen :: Decoupled d1 => SF as bs d1 -> SF bs (E e) d2 -> (e -> SF as bs d3) -> SF as bs (d1 :&: d3)
switchWhen sf sfe f  =  case drep sf of
                          Cau  -> switch (sf >>> forkSecond sfe) f
                          Dec  -> switch (sf >>> forkSecond sfe) f

rswitchWhen :: (Decoupled d1 , Decoupled d3) => SF as bs d1 -> SF bs (E e) d2 -> (e -> SF as bs d3) -> SF as bs (d1 :&: d3)
rswitchWhen sf sfe f =  case (drep sf , drepf f) of
                          (Cau , Cau)  -> rswitch (sf >>> forkSecond sfe) (\ e -> f e >>> forkSecond sfe)
                          (Cau , Dec)  -> rswitch (sf >>> forkSecond sfe) (\ e -> f e >>> forkSecond sfe)
                          (Dec , Cau)  -> rswitch (sf >>> forkSecond sfe) (\ e -> f e >>> forkSecond sfe)
                          (Dec , Dec)  -> rswitch (sf >>> forkSecond sfe) (\ e -> f e >>> forkSecond sfe)

replace :: (Decoupled d1 , Decoupled d2) => SF as bs d1 -> (e -> SF as bs d2) -> SF (as , E e) bs Cau
replace sf f = rswitch (sfFirst sf) (\ e -> sfFirst (f e))

-------------------------------------------------------------------

constantC :: a -> SF as (C a) Dec
constantC a = constantS a >>> fromS

dhold :: a -> SF (E a) (C a) Dec
dhold a = hold a >>> dfromS a

iIntegralS :: Double -> SF (S Double) (C Double) Dec
iIntegralS x = integralS >>> liftC (x +)

iIntegralC :: Double -> SF (C Double) (C Double) Cau
iIntegralC x = integralC >>> liftC (x +)

sampleC :: SF (C a , E b) (E a) Cau
sampleC = sampleWithC const

sampleS :: SF (S a , E b) (E a) Cau
sampleS = sampleWithS const

localTime :: SF as (C Double) Dec
localTime = constantS 1 >>> integralS

after :: Time -> SF as (E ()) Dec
after t = now >>> delayE t

repeatedly :: Time -> SF as (E ()) Dec
repeatedly t = rswitchWhen never (after t) (\ _ -> now)

tag :: a -> SF (E b) (E a) Cau
tag a = liftE (const a)

nowTag :: a -> SF as (E a) Dec
nowTag a = now >>> tag a

afterTag :: Time -> a -> SF as (E a) Dec
afterTag t a = after t >>> tag a

once :: SF (E a) (E a) Cau
once = switch sfFork nowTag

afterEach :: [Dt] -> SF as (E ()) Dec
afterEach [] = never
afterEach (d:des) = switch (never &&& after d) (\ _ -> afterEachAux des)
  where
    afterEachAux :: [Dt] -> SF as (E ()) Dec
    afterEachAux [] = now
    afterEachAux (d:des) = switch (now &&& after d) (\ _ -> afterEachAux des) 

afterEachTag :: [(Dt,e)] -> SF as (E e) Dec
afterEachTag [] = never
afterEachTag ((d,e):des) = switch (never &&& afterTag d e) (afterEachTagAux des)
  where
    afterEachTagAux :: [(Dt,e)] -> e -> SF as (E e) Dec
    afterEachTagAux [] e0 = nowTag e0
    afterEachTagAux ((d,e1):des) e0 = switch (nowTag e0 &&& afterTag d e1) (afterEachTagAux des) 

-------------------------------------------------------------------

symLoop :: Decoupled d => SF (as , cs) (bs , ds) d -> SF ds cs Dec -> SF as bs d
symLoop sff sfb =  case drep sff of
                     Cau  -> loop sff (sfSnd >>> sfb) >>> sfFst
                     Dec  -> loop sff (sfSnd >>> sfb) >>> sfFst

-------------------------------------------------------------------

save :: Decoupled d => SF as bs d -> SF (as , E e) (bs , E (SF as bs d)) Cau
save sf = sfFirst (freeze sf) >>> sfAssocR >>> sfSecond sampleC

saveReplace :: Decoupled d => SF as bs d -> SF ((as , E e) , E (SF as bs d)) (bs , E (SF as bs d)) Cau
saveReplace sf = replace (save sf) save

saveResume :: Decoupled d => SF as bs d -> SF ((as , E a) , E b) bs Cau
saveResume sf = symLoop (sfAssocR >>> sfSecond (sfSwap >>> sampleC) >>> saveReplace sf) (dhold sf)

-------------------------------------------------------------------

fallingBall :: Ball -> SF as (C Ball) Dec
fallingBall (h , v) = constantS (negate g) >>> iIntegralS v >>> forkFirst (iIntegralC h) >>> liftC2 (,)

detectBounce :: SF (C Ball) (E Ball) Cau
detectBounce = when detectImpact

elasticBall :: Ball -> SF as (C Ball) Dec
elasticBall b = rswitchWhen (fallingBall b) detectBounce (fallingBall . negateVel)

inelasticBall :: Ball -> SF as (C Ball) Dec
inelasticBall b = switchWhen (fallingBall b) detectBounce (\ _ -> constantC (0 , 0))

resetBall :: Decoupled d => (Ball -> SF as (C Ball) d) -> Ball -> SF (as , E Ball) (C Ball) Cau
resetBall f b = replace (f b) f

oneInelasticReset :: Ball -> SF (E Ball) (C Ball) Cau
oneInelasticReset b = once >>> sfFork >>> resetBall inelasticBall b

-------------------------------------------------------------------

-- Thomson's Lamp
lamp :: Time -> Bool -> SF as (S Bool) Dec
lamp t b = rswitch (lampAux (t , b)) lampAux
    where
          lampAux :: (Time , Bool) -> SF as (S Bool , E (Time , Bool)) Dec
          lampAux (t' , b') = constantS b' &&& afterTag t' (t' / 2 , not b')

-------------------------------------------------------------------

sampleTime :: SF (E a) (E Time) Cau
sampleTime = forkFirst localTime >>> sampleC

-------------------------------------------------------------------

testBall :: SF (C ()) (C Ball) d -> [Dt] -> [Ball]
testBall sf ts =  let (sf' , b) = step0 sf ()
                  in b : testBall' sf' ts
  where
     testBall' :: SF' (C ()) (C Ball) d -> [Dt] -> [Ball]
     testBall' sf []     = []
     testBall' sf (t:ts) = let (sf' , b) = step' t sf ()
                           in b : testBall' sf' ts
    
-------------------------------------------------------------------
