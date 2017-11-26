{-# LANGUAGE Arrows #-}
module Agent (
    newAgentBehaviour
  ) where

import Model

import FRP.FrABS
import FRP.Yampa

newAgentBehaviour :: Int -> NewAgentBehaviour
newAgentBehaviour initialState = proc (_, e) -> do
  rec
    s' <- iPre initialState -< s
    let s = s' + 1
    
  let ao = agentOutObs s'

  returnA -< (ao, e)
------------------------------------------------------------------------------------------------------------------------
--newAgentBehaviour :: NewAgentBehaviour
--newAgentBehaviour = doRepeatedlyEvery 1.0 (updateAgentStateR (+1))
------------------------------------------------------------------------------------------------------------------------