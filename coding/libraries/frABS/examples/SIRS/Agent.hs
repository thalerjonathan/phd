module SIRS.Agent (
    sirsAgentBehaviour
  ) where

import SIRS.Model

import FRP.FrABS

import Control.Monad.Random
import Control.Monad.Trans.State
import Control.Monad.IfElse

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
isM :: SIRSState -> State SIRSAgentOut Bool
isM sirsStateComp = 
    do
        ss <- domainStateFieldM sirsState
        return $ ss == sirsStateComp

sirsDtM :: Double -> State SIRSAgentOut ()
sirsDtM t =
    do
        whenM (isM Infected) $ handleInfectedAgentM t
        whenM (isM Recovered) $ handleRecoveredAgentM t 

infectAgentM :: Double -> State SIRSAgentOut ()
infectAgentM t =
    do
        doInfect <- drawBoolWithProbFromAgentM infectionProbability
        when doInfect $ updateDomainStateM (\s -> s { sirsState = Infected, sirsTime = t + infectedDuration } )

handleInfectedAgentM :: Double -> State SIRSAgentOut ()
handleInfectedAgentM t = 
    do
        timeOfRecovery <- domainStateFieldM sirsTime
        -- NOTE: if agent has just recovered, don't send infection-contact to others
        ifThenElse (t >= timeOfRecovery) 
                    (updateDomainStateM (\s -> s { sirsState = Recovered, sirsTime = t + immuneDuration} ))
                    randomContactM

handleRecoveredAgentM :: Double -> State SIRSAgentOut ()
handleRecoveredAgentM t = 
    do
        timeOfImmunityLost <- domainStateFieldM sirsTime
        when (t >= timeOfImmunityLost)
                    (updateDomainStateM (\s -> s { sirsState = Susceptible, sirsTime = 0.0 } ))

randomContactM :: State SIRSAgentOut ()
randomContactM = 
    do
        (_, randNeighId) <- pickRandomNeighbourCellM
        sendMessageM (randNeighId, (Contact Infected))

sirsAgentBehaviourFuncM :: Double -> SIRSAgentIn -> State SIRSAgentOut ()
sirsAgentBehaviourFuncM t ain = 
    do
        onMessageMState ain contactInfectedM
        sirsDtM t

    where
        contactInfectedM :: AgentMessage SIRSMsg -> State SIRSAgentOut ()
        contactInfectedM (_, Contact Infected) = whenM (isM Susceptible) (infectAgentM t)
        contactInfectedM _ = return ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR NON-monadic implementation
------------------------------------------------------------------------------------------------------------------------
is :: SIRSAgentOut -> SIRSState -> Bool
is ao ss = (sirsState s) == ss
    where
        s = aoState ao

sirsDt :: SIRSAgentOut -> Double -> SIRSAgentOut
sirsDt ao t
    | is ao Susceptible = ao
    | is ao Infected = handleInfectedAgent ao t
    | otherwise = handleRecoveredAgent ao t

infectAgent :: Double -> SIRSAgentOut -> SIRSAgentOut
infectAgent t ao
    | yes = updateDomainState ao' (\s -> s { sirsState = Infected, sirsTime = t + infectedDuration } )
    | otherwise = ao'
    where
         (yes, ao') = drawBoolWithProbFromAgent ao infectionProbability

contactInfected :: Double -> SIRSAgentOut -> AgentMessage SIRSMsg -> SIRSAgentOut
contactInfected t a (_, Contact Infected) 
    | is a Susceptible = infectAgent t a
    | otherwise = a
contactInfected _ a _ = a

handleInfectedAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleInfectedAgent ao t 
    | t >= timeOfRecovery = recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
    | otherwise = randomContact ao
    where
        timeOfRecovery = sirsTime $ aoState ao
        recoveredAgent = updateDomainState ao (\s -> s { sirsState = Recovered, sirsTime = t + immuneDuration} )

handleRecoveredAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleRecoveredAgent ao t 
    | t >= timeOfImmunityLost = susceptibleAgent
    | otherwise = ao
    where
        timeOfImmunityLost = sirsTime $ aoState ao
        susceptibleAgent = updateDomainState ao (\s -> s { sirsState = Susceptible, sirsTime = 0.0 } )

randomContact :: SIRSAgentOut -> SIRSAgentOut
randomContact ao = sendMessage ao' (randNeigh, (Contact Infected))
    where
        ((_, randNeigh), ao') = runAgentRandom ao (pickRandomNeighbourCell ao)

sirsAgentBehaviourFunc :: Double -> SIRSAgentIn -> SIRSAgentOut -> SIRSAgentOut
sirsAgentBehaviourFunc t ain ao = sirsDt aoAfterMsg t
    where
        aoAfterMsg = onMessage ain (contactInfected t) ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sirsAgentBehaviour :: SIRSAgentBehaviour
sirsAgentBehaviour = agentMonadic sirsAgentBehaviourFuncM  -- agentPure sirsAgentBehaviourFunc
------------------------------------------------------------------------------------------------------------------------