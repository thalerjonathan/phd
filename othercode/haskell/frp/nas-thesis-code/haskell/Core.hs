{-# LANGUAGE
             GADTs,
             KindSignatures,
             EmptyDataDecls,
             ScopedTypeVariables,
             TypeFamilies
    #-}

module Core where

------------------------------------------------------------------------------

import Time
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

---------------------------------------------------------------------------------

-- Nodes --

data Node :: * -> * -> * where
  Node  :: (Dt -> q -> Sample as -> (q , Sample bs)) -> q -> Node as bs

stepNode :: Dt -> Node as bs -> Sample as -> (Node as bs , Sample bs)
stepNode dt (Node f q) sa  = first (Node f) (f dt q sa)

---------------------------------------------------------------------------------

data AtomicRouter :: * -> * -> * where
    SFId  :: AtomicRouter as as
    Fst   :: AtomicRouter (as , bs) as
    Snd   :: AtomicRouter (as , bs) bs

stepRouter :: AtomicRouter as bs -> Sample as -> Sample bs
stepRouter SFId sa         = sa
stepRouter Fst  (sa1 , _)  = sa1
stepRouter Snd  (_ , sa2)  = sa2

---------------------------------------------------------------------------------

data SF :: * -> * -> * where
  Prim     :: (Sample as -> (Node as bs , Sample bs))       -> SF as bs
  ARouter  :: AtomicRouter as bs                            -> SF as bs
  Seq      :: SF as bs -> SF bs cs                          -> SF as cs
  Fan      :: SF as bs -> SF as cs                          -> SF as (bs , cs)
  Switch   :: SF as (bs , E e) -> (e -> SF as bs)           -> SF as bs
  Freeze   :: SF as bs                                      -> SF as (bs , C (SF as bs))
  
data SF' :: * -> * -> * where
  Prim'     :: Node as bs                                   -> SF' as bs
  ARouter'  :: AtomicRouter as bs                           -> SF' as bs
  Seq'      :: SF' as bs -> SF' bs cs                       -> SF' as cs
  Fan'      :: SF' as bs -> SF' as cs                       -> SF' as (bs , cs)
  Switch'   :: SF' as (bs , E e) -> (e -> SF as bs)         -> SF' as bs
  Freeze'   :: SF' as bs                                    -> SF' as (bs , C (SF as bs))
  
---------------------------------------------------------------------------------

step0 :: SF as bs -> Sample as -> (SF' as bs , Sample bs)

step0 (Prim f) sa = first Prim' (f sa)

step0 (ARouter r) sa = (ARouter' r , stepRouter r sa)

step0 (Seq sf1 sf2) sa =  let  (sf1' , sb) = step0 sf1 sa
                               (sf2' , sc) = step0 sf2 sb
                          in (Seq' sf1' sf2' , sc)

step0 (Fan sf1 sf2) sa =  let  (sf1' , sb) = step0 sf1 sa
                               (sf2' , sc) = step0 sf2 sa
                          in (Fan' sf1' sf2' , (sb , sc))

step0 (Switch sf f) sa =  case step0 sf sa of
                            (sf' , (sb , Nothing))  ->  (Switch' sf' f , sb)
                            (_   , (_  , Just e))   ->  step0 (f e) sa

step0 (Freeze sf) sa =  let (sf' , sb) = step0 sf sa
                        in (Freeze' sf' , (sb , sf))

----------------------------------------------------------------------------------------------------

step' :: Dt -> SF' as bs -> Sample as -> (SF' as bs , Sample bs)

step' dt (Prim' n) sa = first Prim' (stepNode dt n sa)

step' dt (ARouter' r) sa = (ARouter' r , stepRouter r sa)

step' dt (Seq' sf1 sf2) sa =  let  (sf1' , sb) = step' dt sf1 sa
                                   (sf2' , sc) = step' dt sf2 sb
                              in (Seq' sf1' sf2' , sc)

step' dt (Fan' sf1 sf2) sa =  let  (sf1' , sb) = step' dt sf1 sa
                                   (sf2' , sc) = step' dt sf2 sa
                              in (Fan' sf1' sf2' , (sb , sc))

step' dt (Switch' sf f) sa =  case step' dt sf sa of
                                (sf' , (sb , Nothing))  -> (Switch' sf' f , sb)
                                (_   , (_  , Just e))   -> step0 (f e) sa

step' dt (Freeze' sf) sa =  let (sf' , sb) = step' dt sf sa
                            in (Freeze' sf' , (sb , freezeSF dt sf))
  where
        freezeSF :: Dt -> SF' as bs -> SF as bs
        freezeSF dt (Prim' n)        = Prim (stepNode dt n)
        freezeSF dt (ARouter' r)     = ARouter r
        freezeSF dt (Seq' sf1 sf2)   = Seq (freezeSF dt sf1) (freezeSF dt sf2)
        freezeSF dt (Fan' sf1 sf2)   = Fan (freezeSF dt sf1) (freezeSF dt sf2)
        freezeSF dt (Switch' sf f)   = Switch (freezeSF dt sf) f
        freezeSF dt (Freeze' sf)     = Freeze (freezeSF dt sf)

----------------------------------------------------------------------------------------------------

runSF ::  forall as bs. SF as bs -> IO (Sample as) -> (Sample bs -> IO ()) -> IO Time -> IO Bool -> IO ()
runSF sf ins outs time done =  do  sa <- ins
                                   let (sf' , sb) = step0 sf sa
                                   outs sb
                                   runSF' 0 sf'
  where  
         runSF' :: Time -> SF' as bs -> IO ()
         runSF' t0 sf' =  do  sa  <- ins
                              t1  <- time
                              let (sf'' , sb) = step' (t1 - t0) sf' sa
                              outs sb
                              d  <- done
                              unless d (runSF' t1 sf'')

----------------------------------------------------------------------------------------------------