{-# LANGUAGE Arrows #-}
module SIRS.SIRSAgent where

import SIRS.SIRSModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
-- TODO: generalize into Agent.hs
updateStateM :: (SIRSAgentState -> SIRSAgentState) -> State SIRSAgentOut ()
updateStateM sfunc = state updateStateMAux
    where
        updateStateMAux :: SIRSAgentOut -> ((), SIRSAgentOut)
        updateStateMAux ao = ((), ao')
            where
                s = aoState ao
                s' = sfunc s
                ao' = ao { aoState = s' }

isM :: SIRSState -> State SIRSAgentOut Bool
isM ss = state isSirsStateMAux
    where
        isSirsStateMAux :: SIRSAgentOut -> (Bool, SIRSAgentOut)
        isSirsStateMAux ao = (flag, ao)
            where
                s = aoState ao
                compSS = sirsState s
                flag = ss == compSS


sirsDtM :: Double -> State SIRSAgentOut ()
sirsDtM dt =
    do
        isSusceptible <- isM Susceptible
        isInfected <- isM Infected

        if isSusceptible then
            return ()
        else if isInfected then
            handleInfectedAgentM dt
        else
            handleRecoveredAgentM dt

infectAgentM :: State SIRSAgentOut ()
infectAgentM =
    do
        infect <- drawBoolWithProbFromAgentM infectionProbability
        if infect then
            updateStateM (\s -> s { sirsState = Infected,
                                      sirsTime = 0.0} )
            else
                updateStateM id

{-
extractStateM :: (SIRSAgentState -> t) -> State SIRSAgentOut t
extractStateM f = state extractStateMAux
    where
        extractStateMAux :: SIRSAgentOut -> (t, SIRSAgentOut)
        extractStateMAux ao = (f s, ao)
            where
                s = aoState ao
-}

handleInfectedAgentM :: Double -> State SIRSAgentOut ()
handleInfectedAgentM dt = 
    do
        -- t <- extractStateM sirsTime
        ao <- get
        let t = sirsTime $ aoState ao
        let t' = t + dt
        if t' >= infectedDuration then
            -- NOTE: agent has just recovered, don't send infection-contact to others
            updateStateM (\s -> s { sirsState = Recovered, sirsTime = 0.0 } )        
            else
                do
                    updateStateM (\s -> s { sirsTime = t' } )
                    randomContactM

handleRecoveredAgentM :: Double -> State SIRSAgentOut ()
handleRecoveredAgentM dt = 
    do
        -- t <- extractStateM sirsTime
        ao <- get
        let t = sirsTime $ aoState ao
        let t' = t + dt
        if t' >= immuneDuration then
            updateStateM (\s -> s { sirsState = Susceptible, sirsTime = 0.0 } )
            else
                updateStateM (\s -> s { sirsTime = t' } )

randomContactM :: State SIRSAgentOut ()
randomContactM = 
    do
        ao <- get
        let env = aoEnv ao
        --coord <- extractStateM sirsCoord
        let coord = sirsCoord $ aoState ao
        let ns = FrABS.Env.Environment.neighbours env coord
        (_, randNeigh) <- agentPickRandomM ns -- TODO: use pickRandomNeighbourCell from AgentUtils
        sendMessageM (randNeigh, (Contact Infected))

sirsAgentBehaviourFuncM :: SIRSAgentIn -> SIRSAgentOut
sirsAgentBehaviourFuncM ain = execState (sirsDtM 1.0) aoAfterMsg
    where
        ao = agentOutFromIn ain
        aoAfterMsg = onMessage ain contactInfected ao
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
    | yes = updateState ao' (\s -> s { sirsState = Infected,
                                      sirsTime = 0.0} )
    | otherwise = ao'
    where
         (ao', yes) = drawInfectionWithProb ao infectionProbability

contactInfected :: SIRSAgentOut -> AgentMessage SIRSMsg -> SIRSAgentOut
contactInfected a (_, Contact Infected) 
    | is a Susceptible = infectAgent a
    | otherwise = a
contactInfected a _ = a

drawInfectionWithProb :: SIRSAgentOut -> Double -> (SIRSAgentOut, Bool)
drawInfectionWithProb ao p = (ao', infect)
    where
        (infectProb, ao') = drawRandomRangeFromAgent ao (0.0, 1.0)
        infect = infectProb <= p

handleInfectedAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleInfectedAgent ao dt = if t' >= infectedDuration then
                                recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
                                else
                                    randomContact gettingBetterAgent

    where
        t = (sirsTime (aoState ao))
        t' = t + dt
        recoveredAgent = updateState ao (\s -> s { sirsState = Recovered,
                                                        sirsTime = 0.0 } )
        gettingBetterAgent = updateState ao (\s -> s { sirsTime = t' } )


handleRecoveredAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleRecoveredAgent ao dt = if t' >= immuneDuration then
                                susceptibleAgent
                                else
                                    immuneReducedAgent
    where
        t = (sirsTime (aoState ao))
        t' = t + dt  -- TODO: use Yampa-function integral
        susceptibleAgent = updateState ao (\s -> s { sirsState = Susceptible,
                                                        sirsTime = 0.0 } )
        immuneReducedAgent = updateState ao (\s -> s { sirsTime = t' } )

-- TODO: include time-semantics e.g. 1 ontact per time-unit
-- TODO: use pickRandomNeighbourCell from AgentUtils
randomContact :: SIRSAgentOut -> SIRSAgentOut
randomContact ao = sendMessage ao' (randNeigh, (Contact Infected))
    where
        ns = FrABS.Env.Environment.neighbours (aoEnv ao) (sirsCoord (aoState ao))
        ((_, randNeigh), ao') = agentPickRandom ao ns

sirsAgentBehaviourFunc :: SIRSAgentIn -> SIRSAgentOut
sirsAgentBehaviourFunc ain = aoAfterTime
    where
        ao = agentOutFromIn ain
        aoAfterMsg = onMessage ain contactInfected ao
        aoAfterTime = sirsDt aoAfterMsg 1.0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sirsAgentBehaviour :: SIRSAgentBehaviour
sirsAgentBehaviour = proc ain ->
    do
        let ao = sirsAgentBehaviourFunc ain
        let aoM = sirsAgentBehaviourFuncM ain
        returnA -< aoM
------------------------------------------------------------------------------------------------------------------------