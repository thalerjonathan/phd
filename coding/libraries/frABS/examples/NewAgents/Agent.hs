{-# LANGUAGE Arrows #-}
module NewAgents.Agent (
    newAgentBehaviour
  ) where

import NewAgents.Model

import FRP.FrABS
import FRP.Yampa

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
newAgentBehaviour' :: NewAgentState -> NewAgentBehaviour
newAgentBehaviour' s = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        
        returnA -< (ao, e)

newAgentBehaviourFunc' :: NewAgentState -> NewAgentIn -> (NewAgentOut, NewAgentState)
newAgentBehaviourFunc' s ain = (ao, s')
    where
        s' = s + 1
        ao = agentOutFromIn ain


newAgentBehaviour :: NewAgentState -> NewAgentBehaviour
newAgentBehaviour s = trace ("blub") (agentPureIgnoreEnv newAgentBehaviourFunc)

newAgentBehaviourFunc :: Double -> NewAgentIn -> NewAgentOut -> NewAgentOut 
newAgentBehaviourFunc _ _ ao = trace ("blob") updateDomainState (\s -> s + 1) ao
------------------------------------------------------------------------------------------------------------------------