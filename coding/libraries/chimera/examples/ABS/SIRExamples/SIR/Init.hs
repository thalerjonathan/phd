module Init
  (
    createSIRNumInfected
  ) where

import System.Random

import Control.Monad.Random
import FRP.Chimera
import FRP.Yampa

import Agent
import Model

createSIRNumInfected :: Int -> Int -> IO ([SIRAgentDef], SIREnvironment)
createSIRNumInfected agentCount numInfected = do
  let agentIds = [0 .. (agentCount-1)]
  let infectedIds = take numInfected agentIds
  let susceptibleIds = drop numInfected agentIds

  adefsSusceptible <- mapM (sirAgent Susceptible) susceptibleIds
  adefsInfected <- mapM (sirAgent Infected) infectedIds

  return (adefsSusceptible ++ adefsInfected, agentIds)

sirAgent :: SIRState -> AgentId -> IO SIRAgentDef
sirAgent initS aid = do
  rng <- newStdGen
  randSt <- evalRandIO $ randomExpM (1 / illnessDuration)

  let st = if initS == Infected 
            then randSt
            else 0

  let s = SIRAgentState {
    sirState     = initS
  , sirStateTime = st
  }

  let adef = AgentDef { 
    adId = aid
  --, adState = s
  , adBeh = sirAgentBehaviour
  , adInitMessages = NoEvent
  --, adConversation = Nothing
  , adRng = rng 
  }

  return adef