module FrSIR.Init 
    (
      createFrSIRNumInfected
    ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import FrSIR.Agent
import FrSIR.Model

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
        , adState = initS
        , adBeh = beh
        , adInitMessages = NoEvent
        , adConversation = Nothing
        , adRng = rng 
        }

    return adef
------------------------------------------------------------------------------------------------------------------------