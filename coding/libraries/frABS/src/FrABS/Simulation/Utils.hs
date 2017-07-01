module FrABS.Simulation.Utils (
  	runAndFreezeSF,
  	freeze,
  	freezeCol,

  	incrementAtomically,
  	incrementAtomicallyUnsafe
  ) where

import FRP.Yampa.InternalCore

import System.IO.Unsafe
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

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

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
incrementAtomically :: TVar Int -> STM Int
incrementAtomically var = 
    do
        value <- readTVar var
        writeTVar var (value + 1)
        return value

incrementAtomicallyUnsafe :: TVar Int -> Int
incrementAtomicallyUnsafe = unsafePerformIO  . atomically . incrementAtomically
