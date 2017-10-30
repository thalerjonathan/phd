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
    receiveHandler (_, Spend amount) = updateAgentStateM (\s -> s + amount)

spend :: PolicyEffectsEnvironment -> Double -> State PolicyEffectsAgentOut ()
spend e amount = do
  wealth <- agentStateM
  when (wealth >= amount) (do
    randNeighbour <- agentRandomNeighbourNode e
    sendMessageM (randNeighbour, Spend amount)
    setAgentStateM $ wealth - amount)

policyEffectsAgentBehaviourM :: PolicyEffectsEnvironment
                                -> Double 
                                -> PolicyEffectsAgentIn 
                                -> State PolicyEffectsAgentOut ()
policyEffectsAgentBehaviourM e _ ain = receive ain >> spend e 1
        
policyEffectsAgentBehaviour :: PolicyEffectsAgentBehaviour
policyEffectsAgentBehaviour = agentMonadicReadEnv policyEffectsAgentBehaviourM
------------------------------------------------------------------------------------------------------------------------