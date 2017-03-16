module FrABS.Utils where

import FRP.Yampa.InternalCore

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- Running a Signal-Function
------------------------------------------------------------------------------------------------------------------------
runAndFreezeSF :: SF a b -> a -> DTime -> (SF a b, b)
runAndFreezeSF sf0 a0 dt = (sfFrozen, b0)
    where
        (sf', b0) = (sfTF sf0) a0
        sfFrozen = freeze sf' dt

-- NOTE: this was taken from Yampa
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}

-- NOTE: this was taken from Yampa
freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (`freeze` dt) sfs
------------------------------------------------------------------------------------------------------------------------