{-# LANGUAGE Arrows #-}
module Agent (
    newAgentBehaviour
  ) where

import Model

import FRP.FrABS
import FRP.Yampa

newAgentBehaviour :: Int -> NewAgentBehaviour
newAgentBehaviour initialState = proc (ain, e) -> do
  rec
    s' <- iPre initialState -< s
    let s = s' + 1
    
  let ao = agentOut s' ain

  returnA -< (ao, e)
------------------------------------------------------------------------------------------------------------------------
--newAgentBehaviour :: NewAgentBehaviour
--newAgentBehaviour = doRepeatedlyEvery 1.0 (updateAgentStateR (+1))
------------------------------------------------------------------------------------------------------------------------