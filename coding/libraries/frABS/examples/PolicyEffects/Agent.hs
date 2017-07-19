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

spend :: PolicyEffectsEnvironment -> Double -> State PolicyEffectsAgentOut ()
spend e amount = 
    do
        wealth <- getDomainStateM
        when (wealth >= amount) 
            (do
                randNeighbour <- agentRandomNeighbourNode e
                sendMessageM (randNeighbour, Spend amount)
                setDomainStateM $ wealth - amount)

policyEffectsAgentBehaviourM :: PolicyEffectsEnvironment
                                -> Double 
                                -> PolicyEffectsAgentIn 
                                -> State PolicyEffectsAgentOut ()
policyEffectsAgentBehaviourM e _ ain = 
    do
        receive ain
        spend e 1

policyEffectsAgentBehaviour :: PolicyEffectsAgentBehaviour
policyEffectsAgentBehaviour = agentMonadicReadEnv policyEffectsAgentBehaviourM
------------------------------------------------------------------------------------------------------------------------