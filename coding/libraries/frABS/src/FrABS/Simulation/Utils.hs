module FrABS.Simulation.Utils (
  	runAndFreezeSF,
  	freeze,
  	freezeCol
  ) where

import FRP.Yampa.InternalCore

------------------------------------------------------------------------------------------------------------------------
-- Running a Signal-Function
------------------------------------------------------------------------------------------------------------------------
runAndFreezeSF :: SF a b -> a -> DTime -> (SF a b, b)
runAndFreezeSF sf0 a0 dt = (sfFrozen, b0)
    where
        (sf', b0) = (sfTF sf0) a0
        sfFrozen = freeze sf' dt

-- TODO: this was taken from Yampa, remove if Yampa exposes it
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}

-- TODO: this was taken from Yampa, remove if Yampa exposes it
freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (`freeze` dt) sfs
------------------------------------------------------------------------------------------------------------------------