{-# LANGUAGE Arrows #-}
module NewAgents.Agent (
    newAgentBehaviour
  ) where

import NewAgents.Model

import FRP.FrABS
import FRP.Yampa

newAgentBehaviour :: Int -> NewAgentBehaviour
newAgentBehaviour initialState = proc (ain, e) -> do
  let ao = agentOutFromIn ain
  
  rec
    s' <- iPre initialState -< s
    let s = s' + 1
    
  let ao' = setAgentState s' ao

  returnA -< (ao', e)
------------------------------------------------------------------------------------------------------------------------
--newAgentBehaviour :: NewAgentBehaviour
--newAgentBehaviour = doRepeatedlyEvery 1.0 (updateAgentStateR (+1))
------------------------------------------------------------------------------------------------------------------------