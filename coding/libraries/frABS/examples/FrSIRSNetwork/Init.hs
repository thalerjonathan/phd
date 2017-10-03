module FrSIRSNetwork.Init (
    createFrSIRSNetworkRandInfected,
    createFrSIRSNetworkNumInfected
  ) where

import FrSIRSNetwork.Model
import FrSIRSNetwork.Agent

import FRP.Yampa

import FRP.FrABS

import Data.List
import System.Random
import Control.Monad.Random

createFrSIRSNetworkRandInfected :: Double 
                                    -> NetworkType
                                    -> IO ([FrSIRSNetworkAgentDef], FrSIRSNetworkEnvironment)
createFrSIRSNetworkRandInfected p network =  
    do
        e <- evalRandIO $ createNetwork network unitEdgeLabeler

        let agentIds = nodesOfNetwork e
        adefs <- mapM (randomFrSIRSNetworkAgent p) agentIds
        
        return (adefs, e)

-- NOTE: numInfected > agentCount = error ("Can't create more infections (" ++ show numInfected ++ ") than there are agents (" ++ show agentCount)
createFrSIRSNetworkNumInfected :: Int 
                                    -> NetworkType
                                    -> IO ([FrSIRSNetworkAgentDef], FrSIRSNetworkEnvironment)
createFrSIRSNetworkNumInfected numInfected network =
    do
        e <- evalRandIO $ createNetwork network unitEdgeLabeler

        let degs = networkDegrees e
        let sortedDegs = Data.List.sortBy highestDegree degs

        let infectedIds = take numInfected $ map fst sortedDegs
        let susceptibleIds = drop numInfected $ map fst sortedDegs

        adefsSusceptible <- mapM (frSIRSNetworkAgent Susceptible) susceptibleIds
        adefsInfected <- mapM (frSIRSNetworkAgent Infected) infectedIds

        return (adefsSusceptible ++ adefsInfected, e)

    where
        highestDegree :: (AgentId, Int) -> (AgentId, Int) -> Ordering
        highestDegree = (\(_, d1) (_, d2) -> compare d2 d1)

        lowestDegree :: (AgentId, Int) -> (AgentId, Int) -> Ordering
        lowestDegree = (\(_, d1) (_, d2) -> compare d1 d2)
        
frSIRSNetworkAgent :: SIRSState
                        -> AgentId
                        -> IO FrSIRSNetworkAgentDef
frSIRSNetworkAgent initS aid = 
    do
        rng <- newStdGen
        let beh = sirsNetworkAgentBehaviour rng initS
        return $ createFrSIRSNetworkDef aid initS beh rng

randomFrSIRSNetworkAgent :: Double
                            -> AgentId
                            -> IO FrSIRSNetworkAgentDef
randomFrSIRSNetworkAgent p aid = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        
        let isInfected = r <= p
        let initS = if isInfected then Infected else Susceptible

        let beh = sirsNetworkAgentBehaviour rng initS

        return $ createFrSIRSNetworkDef aid initS beh rng 

createFrSIRSNetworkDef :: AgentId 
                            -> SIRSState 
                            -> FrSIRSNetworkAgentBehaviour
                            -> StdGen 
                            -> FrSIRSNetworkAgentDef
createFrSIRSNetworkDef aid sirsState beh rng = 
    AgentDef { adId = aid,
                adState = sirsState,
                adBeh = beh,
                adInitMessages = NoEvent,
                adConversation = Nothing,
                adRng = rng }
------------------------------------------------------------------------------------------------------------------------