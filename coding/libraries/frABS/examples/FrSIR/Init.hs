module Init 
  (
    createFrSIRNumInfected
  , sirAgentDefReplicator
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import Agent
import Model

createFrSIRNumInfected :: Int -> Int -> IO ([FrSIRAgentDef], FrSIREnvironment)
createFrSIRNumInfected agentCount numInfected = do
  let agentIds = [0 .. (agentCount-1)]
  let infectedIds = take numInfected agentIds
  let susceptibleIds = drop numInfected agentIds

  adefsSusceptible <- mapM (frSIRAgent Susceptible) susceptibleIds
  adefsInfected <- mapM (frSIRAgent Infected) infectedIds

  return (adefsSusceptible ++ adefsInfected, agentIds)

frSIRAgent :: SIRState -> AgentId -> IO FrSIRAgentDef
frSIRAgent initS aid = do
  rng <- newStdGen
  let beh = sirAgentBehaviour rng initS
  let adef = AgentDef { 
    adId = aid
  , adBeh = beh
  , adInitMessages = NoEvent
  }

  return adef

sirAgentDefReplicator :: Int -> FrSIRAgentDefReplicator
sirAgentDefReplicator numInfected g ad = (ad', g')
  where
    (g', g'') = split g
    s = if (adId ad) < numInfected then Infected else Susceptible 
    -- NOTE: also need to overwrite behaviour with one with a different RNG!
    beh = sirAgentBehaviour g' s
    ad' = ad { adBeh = beh }
------------------------------------------------------------------------------------------------------------------------