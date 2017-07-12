module PolicyEffects.Agent (
    policyEffectsAgentBehaviour
  ) where

import PolicyEffects.Model

import FRP.FrABS

import Control.Monad
import Control.Monad.Trans.State

receive :: PolicyEffectsAgentIn -> State PolicyEffectsAgentOut ()
receive ain = onMessageMState receiveHandler ain
    where
        receiveHandler :: (AgentMessage PolicyEffectsMsg) -> State PolicyEffectsAgentOut ()
        receiveHandler (_, Spend amount) = updateDomainStateM (\s -> s + amount)

spend :: Double -> State PolicyEffectsAgentOut ()
spend amount = 
    do
        wealth <- getDomainStateM
        when (wealth >= amount) (do
                randNeighbour <- pickRandomNeighbourNodeM
                sendMessageM (randNeighbour, Spend amount)
                setDomainStateM $ wealth - amount)

policyEffectsAgentBehaviourM :: Double 
                                -> PolicyEffectsAgentIn 
                                -> State PolicyEffectsAgentOut ()
policyEffectsAgentBehaviourM _ ain = 
    do
        receive ain
        spend 1

policyEffectsAgentBehaviour :: PolicyEffectsAgentBehaviour
policyEffectsAgentBehaviour = agentMonadic policyEffectsAgentBehaviourM
------------------------------------------------------------------------------------------------------------------------