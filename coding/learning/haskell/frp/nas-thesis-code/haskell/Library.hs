{-# LANGUAGE
             GADTs
    #-}

module Library where

import Time
import BouncingBall
import Core
import Primitives

------------------------------------------------------------------

sfSwap :: SF (as , bs) (bs , as)
sfSwap = sfSnd &&& sfFst

toFst :: SF as cs -> SF (as , bs) cs
toFst sf = sfFst >>> sf

toSnd :: SF bs cs -> SF (as , bs) cs
toSnd sf = sfSnd >>> sf

(***) :: SF as cs -> SF bs ds -> SF (as , bs) (cs , ds)
sf1 *** sf2 = toFst sf1 &&& toSnd sf2

sfFirst :: SF as bs -> SF (as , cs) (bs , cs)
sfFirst sf = sf *** identity

sfSecond :: SF bs cs -> SF (as , bs) (as , cs)
sfSecond sf = identity *** sf

sfFork :: SF as (as , as)
sfFork = identity &&& identity

forkFirst :: SF as bs -> SF as (bs , as)
forkFirst sf = sf &&& identity

forkSecond :: SF as bs -> SF as (as , bs)
forkSecond sf = identity &&& sf

sfAssocR :: SF ((as , bs) , cs) (as , (bs , cs))
sfAssocR = toFst sfFst &&& sfFirst sfSnd

sfAssocL :: SF (as , (bs , cs)) ((as , bs) , cs)
sfAssocL = sfSecond sfFst &&& toSnd sfSnd

-------------------------------------------------------------------

rswitch :: SF as (bs , E e) -> (e -> SF as (bs , E e)) -> SF as bs
rswitch sf f = switch sf (\ e -> rswitch (f e >>> sfSecond notYet) f)

switchWhen :: SF as bs -> SF bs (E e) -> (e -> SF as bs) -> SF as bs
switchWhen sf sfe f  = switch (sf >>> forkSecond sfe) f

rswitchWhen :: SF as bs -> SF bs (E e) -> (e -> SF as bs) -> SF as bs
rswitchWhen sf sfe f =  rswitch (sf >>> forkSecond sfe) (\ e -> f e >>> forkSecond sfe)

replace :: SF as bs -> (e -> SF as bs) -> SF (as , E e) bs
replace sf f = rswitch (sfFirst sf) (\ e -> sfFirst (f e))

-------------------------------------------------------------------

constantC :: a -> SF as (C a)
constantC a = constantS a >>> fromS

dhold :: a -> SF (E a) (C a)
dhold a = hold a >>> dfromS a

iIntegralS :: Double -> SF (S Double) (C Double)
iIntegralS x = integralS >>> liftC (x +)

iIntegralC :: Double -> SF (C Double) (C Double)
iIntegralC x = integralC >>> liftC (x +)

sampleC :: SF (C a , E b) (E a)
sampleC = sampleWithC const

sampleS :: SF (S a , E b) (E a)
sampleS = sampleWithS const

localTime :: SF as (C Double)
localTime = constantS 1 >>> integralS

after :: Time -> SF as (E ())
after t = now >>> delayE t

repeatedly :: Time -> SF as (E ())
repeatedly t = rswitchWhen never (after t) (\ _ -> now)

tag :: a -> SF (E b) (E a)
tag a = liftE (const a)

nowTag :: a -> SF as (E a)
nowTag a = now >>> tag a

afterTag :: Time -> a -> SF as (E a)
afterTag t a = after t >>> tag a

once :: SF (E a) (E a)
once = switch sfFork nowTag

afterEach :: [Dt] -> SF as (E ())
afterEach [] = never
afterEach (d:des) = switch (never &&& after d) (\ _ -> afterEachAux des)
  where
    afterEachAux :: [Dt] -> SF as (E ())
    afterEachAux [] = now
    afterEachAux (d:des) = switch (now &&& after d) (\ _ -> afterEachAux des) 

afterEachTag :: [(Dt,e)] -> SF as (E e)
afterEachTag [] = never
afterEachTag ((d,e):des) = switch (never &&& afterTag d e) (afterEachTagAux des)
  where
    afterEachTagAux :: [(Dt,e)] -> e -> SF as (E e)
    afterEachTagAux [] e0 = nowTag e0
    afterEachTagAux ((d,e1):des) e0 = switch (nowTag e0 &&& afterTag d e1) (afterEachTagAux des) 

-------------------------------------------------------------------

save :: SF as bs -> SF (as , E e) (bs , E (SF as bs))
save sf = sfFirst (freeze sf) >>> sfAssocR >>> sfSecond sampleC

saveReplace :: SF as bs -> SF ((as , E e) , E (SF as bs)) (bs , E (SF as bs))
saveReplace sf = replace (save sf) save

-------------------------------------------------------------------

fallingBall :: Ball -> SF as (C Ball)
fallingBall (h , v) = constantS (negate g) >>> iIntegralS v >>> forkFirst (iIntegralC h) >>> liftC2 (,)

detectBounce :: SF (C Ball) (E Ball)
detectBounce = when detectImpact

elasticBall :: Ball -> SF as (C Ball)
elasticBall b = rswitchWhen (fallingBall b) detectBounce (fallingBall . negateVel)

inelasticBall :: Ball -> SF as (C Ball)
inelasticBall b = switchWhen (fallingBall b) detectBounce (\ _ -> constantC (0 , 0))

resetBall :: (Ball -> SF as (C Ball)) -> Ball -> SF (as , E Ball) (C Ball)
resetBall f b = replace (f b) f

oneInelasticReset :: Ball -> SF (E Ball) (C Ball)
oneInelasticReset b = once >>> sfFork >>> resetBall inelasticBall b

-------------------------------------------------------------------

-- Thomson's Lamp
lamp :: Time -> Bool -> SF as (S Bool)
lamp t b = rswitch (lampAux (t , b)) lampAux
    where
          lampAux :: (Time , Bool) -> SF as (S Bool , E (Time , Bool))
          lampAux (t' , b') = constantS b' &&& afterTag t' (t' / 2 , not b')

-------------------------------------------------------------------

sampleTime :: SF (E a) (E Time)
sampleTime = forkFirst localTime >>> sampleC

-------------------------------------------------------------------

testBall :: SF (C ()) (C Ball) -> [Dt] -> [Ball]
testBall sf ts =  let (sf' , b) = step0 sf ()
                  in b : testBall' sf' ts
  where
     testBall' :: SF' (C ()) (C Ball) -> [Dt] -> [Ball]
     testBall' sf []     = []
     testBall' sf (t:ts) = let (sf' , b) = step' t sf ()
                           in b : testBall' sf' ts
                           
-------------------------------------------------------------------
