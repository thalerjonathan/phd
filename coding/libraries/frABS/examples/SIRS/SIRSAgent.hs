{-# LANGUAGE Arrows #-}
module SIRS.SIRSAgent where

import SIRS.SIRSModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils
import FrABS.Env.Environment


import Control.Monad.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Lazy as ST
import Control.Monad (ap)

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- AGENT MONAD TODO: generalize this into Agent.hs
-- combines state- and rand-monad 
------------------------------------------------------------------------------------------------------------------------
-- TODO: can we have elegant updating of domain-state? e.g. in the way we are reading the domain-state?

-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers
-- https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-State-Lazy.html
-- https://github.com/mathandley/haskell_scratch/blob/master/StateIO.hs
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
isM :: SIRSState -> State SIRSAgentOut Bool
isM sirsStateComp = 
    do
        ss <- domainStateFieldM sirsState
        return $ ss == sirsStateComp

sirsDtM :: Double -> State SIRSAgentOut ()
sirsDtM dt =
    do
        isInfected <- isM Infected
        isRecovered <- isM Recovered

        when isInfected $ handleInfectedAgentM dt
        when isRecovered $ handleRecoveredAgentM dt 

infectAgentM :: State SIRSAgentOut ()
infectAgentM =
    do
        doInfect <- drawBoolWithProbFromAgentM infectionProbability
        when doInfect $ updateDomainStateM (\s -> s { sirsState = Infected, sirsTime = 0.0} )

handleInfectedAgentM :: Double -> State SIRSAgentOut ()
handleInfectedAgentM dt = 
    do
        t <- domainStateFieldM sirsTime

        let t' = t + dt
        let hasRecovered = t' >= infectedDuration
        let stillInfected = not hasRecovered

        -- NOTE: agent has just recovered, don't send infection-contact to others
        when hasRecovered $ updateDomainStateM (\s -> s { sirsState = Recovered, sirsTime = 0.0 } )
        when stillInfected $ 
            do 
                updateDomainStateM (\s -> s { sirsTime = t' } )
                randomContactM

handleRecoveredAgentM :: Double -> State SIRSAgentOut ()
handleRecoveredAgentM dt = 
    do
        t <- domainStateFieldM sirsTime

        let t' = t + dt
        let noMoreImmune = t' >= immuneDuration
        let stillImmune = not noMoreImmune

        when noMoreImmune $ updateDomainStateM (\s -> s { sirsState = Susceptible, sirsTime = 0.0 } )
        when stillImmune $ updateDomainStateM (\s -> s { sirsTime = t' } )

randomContactM :: State SIRSAgentOut ()
randomContactM = 
    do
        (_, randNeighId) <- pickRandomNeighbourCellM
        sendMessageM (randNeighId, (Contact Infected))

sirsAgentBehaviourFuncM :: SIRSAgentIn -> State SIRSAgentOut ()
sirsAgentBehaviourFuncM ain = 
    do
        onMessageM ain contactInfectedM
        sirsDtM 1.0

    where
        contactInfectedM :: AgentMessage SIRSMsg -> State SIRSAgentOut ()
        contactInfectedM (_, Contact Infected) =
            do
                isSusceptible <- isM Susceptible
                when isSusceptible infectAgentM
        contactInfectedM _ = return ()

------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR YAMPA implementation
------------------------------------------------------------------------------------------------------------------------
sirsAgentSuceptibleSF :: RandomGen g => g -> SIRSAgentBehaviour
sirsAgentSuceptibleSF g = switch 
                            sirsAgentSusceptibleBehaviourSF
                            (sirsAgentSusceptibleInfectedSF g)

sirsAgentSusceptibleBehaviourSF :: SF SIRSAgentIn (SIRSAgentOut, Event ())
sirsAgentSusceptibleBehaviourSF = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = updateDomainState ao (\s -> s { sirsState = Susceptible})

        infectionEvent <- (iEdge False) -< hasMessage ain (Contact Infected)

        returnA -< (ao', infectionEvent)

-- TODO: update sirsState to infected here once, no need to constantly set to infected in infecedbehaviourSF
sirsAgentSusceptibleInfectedSF :: RandomGen g => g -> () -> SIRSAgentBehaviour
sirsAgentSusceptibleInfectedSF g _ = sirsAgentInfectedSF g



sirsAgentInfectedSF :: RandomGen g => g -> SIRSAgentBehaviour
sirsAgentInfectedSF g = switch 
                            (sirsAgentInfectedBehaviourSF g)
                            (sirsAgentInfectedRecoveredSF g)


sirsAgentInfectedBehaviourSF :: RandomGen g => g -> SF SIRSAgentIn (SIRSAgentOut, Event ())
sirsAgentInfectedBehaviourSF g = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = updateDomainState ao (\s -> s { sirsState = Infected})

        remainingInfectedTime <- (infectedDuration-) ^<< integral -< 1.0
        recoveredEvent <- edge -< (remainingInfectedTime <= 0)

        -- NOTE: this means the agent is randomly contacting one neighbour within the infected duration
        makeContact <- occasionally g (infectedDuration * 0.5) () -< ()

        let ao'' = if isEvent makeContact then
                    randomContact ao'
                        else
                            ao'

        returnA -< (ao'', recoveredEvent)

-- TODO: update sirsState to recovered here once, no need to constantly set to recovered in recoverbehaviourSF
sirsAgentInfectedRecoveredSF :: RandomGen g => g -> () -> SIRSAgentBehaviour
sirsAgentInfectedRecoveredSF g _ = sirsAgentRecoveredSF g



sirsAgentRecoveredSF :: RandomGen g => g -> SIRSAgentBehaviour
sirsAgentRecoveredSF g = switch 
                            sirsAgentRecoveredBehaviourSF
                            (sirsAgentRecoveredSusceptibleSF g)

sirsAgentRecoveredBehaviourSF :: SF SIRSAgentIn (SIRSAgentOut, Event ())
sirsAgentRecoveredBehaviourSF = proc ain ->
    do
        let ao = agentOutFromIn ain
        let ao' = updateDomainState ao (\s -> s { sirsState = Recovered})

        remainingImmuneTime <- (immuneDuration-) ^<< integral -< 1.0
        backToSusceptibleEvent <- edge -< (remainingImmuneTime <= 0)

        returnA -< (ao', backToSusceptibleEvent)

-- TODO: update sirsState to susceptible here once, no need to constantly set to susceptible in susceptiblebehaviourSF
sirsAgentRecoveredSusceptibleSF :: RandomGen g => g -> () -> SIRSAgentBehaviour
sirsAgentRecoveredSusceptibleSF g _ = sirsAgentSuceptibleSF g

sirsAgentBehaviourSF :: RandomGen g => g -> SIRSState -> SIRSAgentBehaviour
sirsAgentBehaviourSF g Susceptible = sirsAgentSuceptibleSF g
sirsAgentBehaviourSF g Infected = sirsAgentInfectedSF g
sirsAgentBehaviourSF g Recovered = sirsAgentRecoveredSF g
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR NON-monadic implementation
------------------------------------------------------------------------------------------------------------------------
is :: SIRSAgentOut -> SIRSState -> Bool
is ao ss = (sirsState s) == ss
    where
        s = aoState ao

-- TODO: use switching SFs when in different states as behaviour changes
sirsDt :: SIRSAgentOut -> Double -> SIRSAgentOut
sirsDt ao dt
    | is ao Susceptible = ao
    | is ao Infected = handleInfectedAgent ao dt
    | otherwise = handleRecoveredAgent ao dt

infectAgent :: SIRSAgentOut -> SIRSAgentOut
infectAgent ao
    | yes = updateDomainState ao' (\s -> s { sirsState = Infected,
                                      sirsTime = 0.0} )
    | otherwise = ao'
    where
         (yes, ao') = drawBoolWithProbFromAgent ao infectionProbability

contactInfected :: SIRSAgentOut -> AgentMessage SIRSMsg -> SIRSAgentOut
contactInfected a (_, Contact Infected) 
    | is a Susceptible = infectAgent a
    | otherwise = a
contactInfected a _ = a

handleInfectedAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleInfectedAgent ao dt = if t' >= infectedDuration then
                                recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
                                else
                                    randomContact gettingBetterAgent

    where
        t = (sirsTime (aoState ao))
        t' = t + dt
        recoveredAgent = updateDomainState ao (\s -> s { sirsState = Recovered,
                                                        sirsTime = 0.0 } )
        gettingBetterAgent = updateDomainState ao (\s -> s { sirsTime = t' } )


handleRecoveredAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleRecoveredAgent ao dt = if t' >= immuneDuration then
                                susceptibleAgent
                                else
                                    immuneReducedAgent
    where
        t = (sirsTime (aoState ao))
        t' = t + dt  -- TODO: use Yampa-function integral
        susceptibleAgent = updateDomainState ao (\s -> s { sirsState = Susceptible,
                                                        sirsTime = 0.0 } )
        immuneReducedAgent = updateDomainState ao (\s -> s { sirsTime = t' } )

-- TODO: include time-semantics e.g. 1 ontact per time-unit
randomContact :: SIRSAgentOut -> SIRSAgentOut
randomContact ao = sendMessage ao' (randNeigh, (Contact Infected))
    where
        ((_, randNeigh), ao') = runAgentRandom ao (pickRandomNeighbourCell ao)

sirsAgentBehaviourFunc :: SIRSAgentIn -> SIRSAgentOut -> SIRSAgentOut
sirsAgentBehaviourFunc ain ao = aoAfterTime
    where
        aoAfterMsg = onMessage ain contactInfected ao
        aoAfterTime = sirsDt aoAfterMsg 1.0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sirsAgentBehaviour :: SIRSAgentBehaviour
sirsAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain 
        --let ao' = sirsAgentBehaviourFunc ain ao
        let ao' = execState (sirsAgentBehaviourFuncM ain) ao
        returnA -< ao'
------------------------------------------------------------------------------------------------------------------------
