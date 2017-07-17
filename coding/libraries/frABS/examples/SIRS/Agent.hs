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

sirsDtM :: SIRSEnvironment -> Double -> State SIRSAgentOut ()
sirsDtM e t =
    do
        whenM (isM Infected) (handleInfectedAgentM e t)
        whenM (isM Recovered) (handleRecoveredAgentM t)

infectAgentM :: Double -> State SIRSAgentOut ()
infectAgentM t =
    do
        doInfect <- drawBoolWithProbFromAgentM infectionProbability
        when doInfect $ updateDomainStateM (\s -> s { sirsState = Infected, sirsTime = t + infectedDuration } )

handleInfectedAgentM :: SIRSEnvironment -> Double -> State SIRSAgentOut ()
handleInfectedAgentM e t = 
    do
        timeOfRecovery <- domainStateFieldM sirsTime
        -- NOTE: if agent has just recovered, don't send infection-contact to others
        ifThenElse (t >= timeOfRecovery) 
                    (updateDomainStateM (\s -> s { sirsState = Recovered, sirsTime = t + immuneDuration} ))
                    (randomContactM e)

handleRecoveredAgentM :: Double -> State SIRSAgentOut ()
handleRecoveredAgentM t = 
    do
        timeOfImmunityLost <- domainStateFieldM sirsTime
        when (t >= timeOfImmunityLost)
                    (updateDomainStateM (\s -> s { sirsState = Susceptible, sirsTime = 0.0 } ))

randomContactM :: SIRSEnvironment -> State SIRSAgentOut ()
randomContactM e = 
    do
        coord <- domainStateFieldM sirsCoord
        (_, randNeighId) <- runAgentRandomM (pickRandomNeighbourCell coord e)
        sendMessageM (randNeighId, (Contact Infected))

sirsAgentBehaviourFuncM :: SIRSAgentMonadicReadEnvBehaviour
sirsAgentBehaviourFuncM e t ain = 
    do
        onMessageMState contactInfectedM ain
        sirsDtM e t

    where
        contactInfectedM :: AgentMessage SIRSMsg -> State SIRSAgentOut ()
        contactInfectedM (_, Contact Infected) = whenM (isM Susceptible) (infectAgentM t)
        contactInfectedM _ = return ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR NON-monadic implementation
------------------------------------------------------------------------------------------------------------------------
is :: SIRSState -> SIRSAgentOut -> Bool
is ss ao = (sirsState s) == ss
    where
        s = aoState ao

sirsDt :: SIRSEnvironment -> Double -> SIRSAgentOut -> SIRSAgentOut
sirsDt e t ao
    | is Susceptible ao = ao
    | is Infected ao = handleInfectedAgent e t ao
    | otherwise = handleRecoveredAgent t ao

infectAgent :: Double -> SIRSAgentOut -> SIRSAgentOut
infectAgent t ao
    | yes = updateDomainState (\s -> s { sirsState = Infected, sirsTime = t + infectedDuration }) ao'
    | otherwise = ao'
    where
         (yes, ao') = drawBoolWithProbFromAgent infectionProbability ao

contactInfected :: Double -> AgentMessage SIRSMsg -> SIRSAgentOut -> SIRSAgentOut
contactInfected t (_, Contact Infected) ao
    | is Susceptible ao = infectAgent t ao
    | otherwise = ao
contactInfected _ _ ao = ao

handleInfectedAgent :: SIRSEnvironment -> Double -> SIRSAgentOut -> SIRSAgentOut
handleInfectedAgent e t ao
    | t >= timeOfRecovery = recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
    | otherwise = randomContact e ao
    where
        timeOfRecovery = sirsTime $ aoState ao
        recoveredAgent = updateDomainState (\s -> s { sirsState = Recovered, sirsTime = t + immuneDuration}) ao

handleRecoveredAgent :: Double -> SIRSAgentOut -> SIRSAgentOut
handleRecoveredAgent t ao 
    | t >= timeOfImmunityLost = susceptibleAgent
    | otherwise = ao
    where
        timeOfImmunityLost = sirsTime $ aoState ao
        susceptibleAgent = updateDomainState (\s -> s { sirsState = Susceptible, sirsTime = 0.0 }) ao

randomContact :: SIRSEnvironment -> SIRSAgentOut -> SIRSAgentOut
randomContact e ao = sendMessage (randNeigh, Contact Infected) ao'
    where
        coord = sirsCoord $ aoState ao
        ((_, randNeigh), ao') = runAgentRandom (pickRandomNeighbourCell coord e) ao

sirsAgentBehaviourFunc :: SIRSAgentPureReadEnvBehaviour
sirsAgentBehaviourFunc e t ain ao = ao'
    where
        aoAfterMsg = onMessage (contactInfected t) ain ao
        ao' = sirsDt e t aoAfterMsg
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sirsAgentBehaviour :: SIRSAgentBehaviour
sirsAgentBehaviour = agentMonadicReadEnv sirsAgentBehaviourFuncM  -- agentPureReadEnv sirsAgentBehaviourFunc
------------------------------------------------------------------------------------------------------------------------