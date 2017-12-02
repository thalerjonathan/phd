module Agent 
  (
    sirAgentBehaviour
  ) where

import Control.Monad.IfElse
import Control.Monad.Random
import Control.Monad.Trans.State
import FRP.Chimera

import Model

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
isM :: SIRState -> State SIRAgentOut Bool
isM ss = agentStateFieldM sirState >>= (\ss' -> return (ss == ss'))

susceptibleAgentM :: SIREnvironment -> Double -> SIRAgentIn -> State SIRAgentOut ()
susceptibleAgentM e t ain = do
    onMessageMState contactByInfectedM ain
    randContacts <- agentRandomM (randomExpM (1 / contactRate))
    forM_ [0..randContacts] (\_ -> (randomContactM (agentId ain) Susceptible e))

  where
    contactByInfectedM :: AgentMessage SIRMsg -> State SIRAgentOut ()
    contactByInfectedM (_, Contact Infected) = infectM t
    contactByInfectedM _ = return ()

    infectM :: Double -> State SIRAgentOut ()
    infectM t = do
      doInfect <- agentRandomBoolProbM infectionProbability
      expInfectedDuration <- agentRandomM (randomExpM (1 / illnessDuration))
      when doInfect $ updateAgentStateM (\s -> s { sirState = Infected, sirStateTime = t + expInfectedDuration })

    randomContactM :: AgentId -> SIRState -> SIREnvironment -> State SIRAgentOut ()
    randomContactM aid state e = do
      randNeighId <- agentRandomPickM e
      if randNeighId == aid
        then randomContactM aid state e
        else sendMessageM (randNeighId, Contact state)

infectedAgentM :: Double -> SIRAgentIn -> State SIRAgentOut ()
infectedAgentM t ain = do
  timeOfRecovery <- agentStateFieldM sirStateTime
  if t >= timeOfRecovery
    then (updateAgentStateM (\s -> s { sirState = Recovered, sirStateTime = 0}))
    else (respondToContactWithM Infected ain)

respondToContactWithM :: SIRState -> SIRAgentIn -> State SIRAgentOut ()
respondToContactWithM state ain = onMessageMState respondToContactWithAux ain
  where
    respondToContactWithAux :: AgentMessage SIRMsg -> State SIRAgentOut ()
    respondToContactWithAux (senderId, Contact _) = sendMessageM (senderId, Contact state)

sirAgentBehaviourFuncM :: SIRAgentMonadicReadEnvBehaviour
sirAgentBehaviourFuncM e t ain = do
  ifThenElseM (isM Susceptible) 
    (susceptibleAgentM e t ain)
    (whenM (isM Infected) (infectedAgentM t ain))
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR NON-monadic implementation
------------------------------------------------------------------------------------------------------------------------
is :: SIRState -> SIRAgentOut -> Bool
is ss ao = (sirState $ agentState ao) == ss

susceptibleAgent :: SIREnvironment -> Double -> SIRAgentIn -> SIRAgentOut -> SIRAgentOut
susceptibleAgent e t ain ao = foldr (\_ -> randomContact Susceptible e) ao'' [0..randContacts] 
  where
    ao' = onMessage (contactByInfected t) ain ao
    (randContacts, ao'') = agentRandom (randomExpM (1 / contactRate)) ao'

    contactByInfected :: Double -> AgentMessage SIRMsg -> SIRAgentOut -> SIRAgentOut
    contactByInfected t (_, Contact Infected) ao = infect t ao
    contactByInfected _ _ ao = ao

    infect :: Double -> SIRAgentOut -> SIRAgentOut
    infect t ao
        | yes = updateAgentState (\s -> s { sirState = Infected, sirStateTime = t + expInfectedDuration }) ao''
        | otherwise = ao'
      where
          (yes, ao') = agentRandomBoolProb infectionProbability ao
          (expInfectedDuration, ao'') = agentRandom (randomExpM (1 / illnessDuration)) ao'

    randomContact :: SIRState -> SIREnvironment -> SIRAgentOut -> SIRAgentOut
    randomContact s e ao 
        | aid == randNeigh = randomContact s e ao'
        | otherwise = sendMessage (randNeigh, Contact s) ao'
      where
        aid = agentId ain
        (randNeigh, ao') = agentRandomPick e ao
        
infectedAgent :: Double -> SIRAgentIn -> SIRAgentOut -> SIRAgentOut
infectedAgent t ain ao
    | t >= timeOfRecovery = recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
    | otherwise = respondToContactWith Infected ain ao
  where
    timeOfRecovery = sirStateTime $ agentState ao
    recoveredAgent = updateAgentState (\s -> s { sirState = Recovered, sirStateTime = 0}) ao

    respondToContactWith :: SIRState -> SIRAgentIn -> SIRAgentOut -> SIRAgentOut
    respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
      where
        respondToContactWithAux :: AgentMessage SIRMsg -> SIRAgentOut -> SIRAgentOut
        respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao

sirAgentBehaviourFunc :: SIRState -> SIRAgentPureReadEnvBehaviour
sirAgentBehaviourFunc s e t ain 
    | is Susceptible ao = susceptibleAgent e t ain ao
    | is Infected ao = infectedAgent t ain ao
    | otherwise = ao
  where
    ao = agentOut 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sirAgentBehaviour :: SIRState -> SIRAgentBehaviour
sirAgentBehaviour s = agentPureReadEnv (sirAgentBehaviourFunc s) -- agentMonadicReadEnv sirAgentBehaviourFuncM  -- agentPureReadEnv sirAgentBehaviourFunc
------------------------------------------------------------------------------------------------------------------------