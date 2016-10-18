{-# LANGUAGE
             GADTs,
             KindSignatures,
             TypeOperators,
             EmptyDataDecls,
             ScopedTypeVariables,
             TypeFamilies
    #-}

module CoreD where

------------------------------------------------------------------------------

import Time
import Decoupled
import ArrowCombinators
import Control.Arrow
import Control.Monad

------------------------------------------------------------------------------

-- Signal Time Domains

data C a :: *
data E a :: *
data S a :: *

type family   Sample as         :: *
type instance Sample (C a)      =  a
type instance Sample (E a)      =  Maybe a
type instance Sample (S a)      =  a
type instance Sample (as , bs)  =  (Sample as , Sample bs)

-------------------------------------------------------------------------------

-- Nodes --

data Node :: * -> * -> * -> * where
  CNode  :: (Dt -> q ->   Sample as -> (q , Sample bs)) -> q -> Node as bs Cau
  DNode  :: (Dt -> q -> ((Sample as -> q) , Sample bs)) -> q -> Node as bs Dec

stepNode :: Dt -> Node as bs d -> Sample as -> (Node as bs d , Sample bs)
stepNode dt (CNode f q) sa  = first (CNode f) (f dt q sa)
stepNode dt (DNode f q) sa  = first (\ g -> DNode f (g sa)) (f dt q)

dstepNode :: Dt -> Node as bs Dec -> ((Sample as -> Node as bs Dec) , Sample bs)
dstepNode dt (DNode f q) = first (\ g sa -> DNode f (g sa)) (f dt q)

---------------------------------------------------------

data AtomicRouter :: * -> * -> * where
    SFId  :: AtomicRouter as as
    Fst   :: AtomicRouter (as , bs) as
    Snd   :: AtomicRouter (as , bs) bs

stepARouter :: AtomicRouter as bs -> Sample as -> Sample bs
stepARouter SFId sa         = sa
stepARouter Fst  (sa1 , _)  = sa1
stepARouter Snd  (_ , sa2)  = sa2

-------------------------------------------------------------------------------

-- Signal Function Representation --

data SF :: * -> * -> * -> * where
  CPrim    :: (Sample as -> (Node as bs Cau , Sample bs))                -> SF as bs Cau
  DPrim    :: (Sample as -> Node as bs Dec) -> Sample bs                 -> SF as bs Dec
  ARouter  :: AtomicRouter as bs                                         -> SF as bs Cau
  Seq      :: Decoupled d1 => SF as bs d1 -> SF bs cs d2                 -> SF as cs (d1 :|: d2)
  Fan      :: Decoupled d1 => SF as bs d1 -> SF as cs d2                 -> SF as (bs , cs) (d1 :&: d2)
  Switch   :: Decoupled d1 => SF as (bs , E e) d1 -> (e -> SF as bs d2)  -> SF as bs (d1 :&: d2)
  Freeze   :: SF as bs d                                                 -> SF as (bs , C (SF as bs d)) d
  Loop     :: SF (as , cs) bs d -> SF bs cs Dec                          -> SF as bs d
  Weaken   :: SF as bs d                                                 -> SF as bs Cau

data SF' :: * -> * -> * -> * where
  Prim'     :: Decoupled d => Node as bs d                                 -> SF' as bs d
  ARouter'  :: AtomicRouter as bs                                          -> SF' as bs Cau
  Seq'      :: Decoupled d1 => SF' as bs d1 -> SF' bs cs d2                -> SF' as cs (d1 :|: d2)
  Fan'      :: Decoupled d1 => SF' as bs d1 -> SF' as cs d2                -> SF' as (bs , cs) (d1 :&: d2)
  Switch'   :: Decoupled d1 => SF' as (bs , E e) d1 -> (e -> SF as bs d2)  -> SF' as bs (d1 :&: d2)
  Freeze'   :: SF' as bs d                                                 -> SF' as (bs , C (SF as bs d)) d
  Loop'     :: SF' (as , cs) bs d -> SF' bs cs Dec                         -> SF' as bs d
  Weaken'   :: SF' as bs d                                                 -> SF' as bs Cau

----------------------------------------------------------------------------------------------------

class Decoupled d where
  drep    :: SF as bs d -> DRep d
  drepf   :: (e -> SF as bs d) -> DRep d
  drep'   :: SF' as bs d -> DRep d

instance Decoupled Cau where
  drep  _ = Cau
  drepf _ = Cau
  drep' _ = Cau

instance Decoupled Dec where
  drep  _ = Dec
  drepf _ = Dec
  drep' _ = Dec

----------------------------------------------------------------------------------------------------

weakenSwitch :: DRep d1 -> SF' as bs d2 -> SF' as bs (d1 :&: d2)
weakenSwitch Cau sf = Weaken' sf
weakenSwitch Dec sf = sf

----------------------------------------------------------------------------------------------------

step0 :: SF as bs d -> Sample as -> (SF' as bs d , Sample bs)

step0 (CPrim f) sa = first Prim' (f sa)

step0 (DPrim f sb) sa = (Prim' (f sa) , sb)

step0 (ARouter r) sa = (ARouter' r , stepARouter r sa)

step0 (Seq sf1 sf2) sa =  let  (sf1' , sb) = step0 sf1 sa
                               (sf2' , sc) = step0 sf2 sb
                          in (Seq' sf1' sf2' , sc)

step0 (Fan sf1 sf2) sa =  let  (sf1' , sb) = step0 sf1 sa
                               (sf2' , sc) = step0 sf2 sa
                          in (Fan' sf1' sf2' , (sb , sc))

step0 (Switch sf f) sa =  case step0 sf sa of
                            (sf' , (sb , Nothing))  ->  (Switch' sf' f , sb)
                            (_   , (_  , Just e))   ->  first (weakenSwitch (drep sf)) (step0 (f e) sa)

step0 (Freeze sf) sa =  let (sf' , sb) = step0 sf sa
                        in (Freeze' sf' , (sb , sf))

step0 (Loop sff sfb) sa =  case dstep0 sfb of 
                             (g , sc) ->  case step0 sff (sa , sc) of
                                            (sff' , sb) -> (Loop' sff' (g sb) , sb)

step0 (Weaken sf) sa = first Weaken' (step0 sf sa)


dstep0 :: SF as bs Dec -> ((Sample as -> SF' as bs Dec) , Sample bs)

dstep0 (DPrim f sb) = (Prim' . f , sb)

dstep0 (Seq sf1 sf2) =  case drep sf1 of
                          Dec ->  let  (g , sb)     = dstep0 sf1
                                       (sf2' , sc)  = step0 sf2 sb
                                  in ((\ sa -> Seq' (g sa) sf2') , sc)
                          Cau ->  let (g , sc) = dstep0 sf2
                                  in  (\ sa ->  let (sf1' , sb) = step0 sf1 sa
                                                in Seq' sf1' (g sb)
                                      , sc)

dstep0 (Fan sf1 sf2) =  case drep sf1 of
                          Dec ->  let  (g1 , sb) = dstep0 sf1
                                       (g2 , sc) = dstep0 sf2
                                  in ((\ sa -> Fan' (g1 sa) (g2 sa)) , (sb , sc))

dstep0 (Switch sf f) =  case drep sf of
                          Dec ->  case dstep0 sf of
                                    (g , (sb , Nothing))  -> ((\ sa -> Switch' (g sa) f) , sb)
                                    (_ , (_  , Just e))   -> dstep0 (f e)

dstep0 (Freeze sf) =  let (g , sb) = dstep0 sf
                      in (Freeze' . g , (sb , sf))


dstep0 (Loop sff sfb) =  case dstep0 sff of
                           (g , sb) ->  case step0 sfb sb of
                                          (sfb' , sc) -> ((\ sa -> Loop' (g (sa , sc)) sfb') , sb)

----------------------------------------------------------------------------------------------------

freezeSF :: Dt -> SF' as bs d -> SF as bs d
freezeSF dt (ARouter' r)     = ARouter r
freezeSF dt (Seq' sf1 sf2)   = Seq (freezeSF dt sf1) (freezeSF dt sf2)
freezeSF dt (Fan' sf1 sf2)   = Fan (freezeSF dt sf1) (freezeSF dt sf2)
freezeSF dt (Switch' sf f)   = Switch (freezeSF dt sf) f
freezeSF dt (Freeze' sf)     = Freeze (freezeSF dt sf)
freezeSF dt (Loop' sff sfb)  = Loop (freezeSF dt sff) (freezeSF dt sfb)
freezeSF dt (Weaken' sf)     = Weaken (freezeSF dt sf)
freezeSF dt (Prim' n)        =  case n of
                                  CNode _ _ -> CPrim (stepNode dt n)
                                  DNode _ _ -> uncurry DPrim (dstepNode dt n)

----------------------------------------------------------------------------------------------------

step' :: Dt -> SF' as bs d -> Sample as -> (SF' as bs d , Sample bs)

step' dt (Prim' n) sa = first Prim' (stepNode dt n sa)

step' dt (ARouter' r) sa = (ARouter' r , stepARouter r sa)

step' dt (Seq' sf1 sf2) sa =  let  (sf1' , sb) = step' dt sf1 sa
                                   (sf2' , sc) = step' dt sf2 sb
                              in (Seq' sf1' sf2' , sc)

step' dt (Fan' sf1 sf2) sa =  let  (sf1' , sb) = step' dt sf1 sa
                                   (sf2' , sc) = step' dt sf2 sa
                              in (Fan' sf1' sf2' , (sb , sc))

step' dt (Switch' sf f) sa =  case step' dt sf sa of
                                (sf' , (sb , Nothing))  -> (Switch' sf' f , sb)
                                (_   , (_  , Just e))   -> first (weakenSwitch (drep' sf)) (step0 (f e) sa)

step' dt (Freeze' sf) sa =  let (sf' , sb) = step' dt sf sa
                            in (Freeze' sf' , (sb , freezeSF dt sf))

step' dt (Loop' sff sfb) sa =  case dstep' dt sfb of
                                 (g , sc) ->  case step' dt sff (sa , sc) of
                                                (sff' , sb) -> (Loop' sff' (g sb) , sb)

step' dt (Weaken' sf) sa = first Weaken' (step' dt sf sa)


dstep' :: Dt -> SF' as bs Dec -> ((Sample as -> SF' as bs Dec) , Sample bs)

dstep' dt (Prim' n) = (first . result) Prim' (dstepNode dt n)

dstep' dt (Seq' sf1 sf2) =  case drep' sf1 of
                              Dec ->  let  (g , sb)    = dstep' dt sf1
                                           (sf2' , sc) = step' dt sf2 sb
                                      in ((\ sa -> Seq' (g sa) sf2') , sc)

                              Cau ->  let (g , sc) = dstep' dt sf2
                                      in  (\ sa ->  let  (sf1' , sb) = step' dt sf1 sa
                                                    in   Seq' sf1' (g sb)
                                          , sc)


dstep' dt (Fan' sf1 sf2) =  case drep' sf1 of
                              Dec ->  let  (g1 , sb) = dstep' dt sf1
                                           (g2 , sc) = dstep' dt sf2
                                      in ((\ sa -> Fan' (g1 sa) (g2 sa)) , (sb , sc))

dstep' dt (Switch' sf f) =  case drep' sf of
                              Dec ->  case dstep' dt sf of
                                        (g , (sb , Nothing))  -> ((\ sa -> Switch' (g sa) f) , sb)
                                        (_ , (_  , Just e))   -> dstep0 (f e)

dstep' dt (Freeze' sf) =  let (g , sb) = dstep' dt sf
                          in (Freeze' . g , (sb , freezeSF dt sf))


dstep' dt (Loop' sff sfb) = case dstep' dt sff of
                              (g , sb) ->  case step' dt sfb sb of
                                             (sfb' , sc) -> ((\ sa -> Loop' (g (sa , sc)) sfb') , sb)

---------------------------------------------------------

runSF ::  forall as bs d. 
          SF as bs d -> IO (Sample as) -> (Sample bs -> IO ()) -> IO Time -> IO Bool -> IO ()
runSF sf ins outs time done =  do  sa <- ins
                                   let (sf' , sb) = step0 sf sa
                                   outs sb
                                   runSF' 0 sf'
  where  
         runSF' :: Time -> SF' as bs d -> IO ()
         runSF' t0 sf' =  do  sa  <- ins
                              t1  <- time
                              let (sf'' , sb) = step' (t1 - t0) sf' sa
                              outs sb
                              d  <- done
                              unless d (runSF' t1 sf'')

---------------------------------------------------------
