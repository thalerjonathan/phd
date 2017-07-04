module FrSIRSNetwork.FrSIRSNetworkInit where

import FrSIRSNetwork.FrSIRSNetworkModel
import FrSIRSNetwork.FrSIRSNetworkAgent

import Utils.Utils

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Env.EnvironmentUtils

import Data.List
import Data.Ord
import System.Random
import Control.Monad.Random

import Debug.Trace

createFrSIRSNetworkRandInfected :: (Int, Int) 
                                    -> Double 
                                    -> NetworkType
                                    -> IO ([FrSIRSNetworkAgentDef], FrSIRSNetworkEnvironment)
createFrSIRSNetworkRandInfected dims@(maxX, maxY) p network =  
    do
        rng <- newStdGen
        gr <- evalRandIO $ createAgentNetwork network

        let agentIds = nodesOfNetwork gr
        adefs <- mapM (randomFrSIRSNetworkAgent p) (zip coords agentIds)
        
        let env = (createEnvironment
                        Nothing
                        dims
                        moore
                        ClipToMax
                        []
                        rng
                        (Just gr))

        return (adefs, env)

    where
        agentCount = maxX * maxY
        coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]

createFrSIRSNetworkNumInfected :: (Int, Int) 
                                    -> Int 
                                    -> NetworkType
                                    -> IO ([FrSIRSNetworkAgentDef], FrSIRSNetworkEnvironment)
createFrSIRSNetworkNumInfected dims@(maxX, maxY) numInfected network
    | numInfected > agentCount = error ("Can't create more infections (" ++ (show numInfected) ++ ") than there are agents (" ++ (show agentCount))
    | otherwise =
        do
            rng <- newStdGen
            gr <- evalRandIO $ createAgentNetwork network

            let degs = networkDegrees gr
            let sortedDegs = Data.List.sortBy highestDegree degs

            let infectedIds = take numInfected $ map fst sortedDegs
            let susceptibleIds = drop numInfected $ map fst sortedDegs

            let infectedCoords = take (length infectedIds) coords
            let susceptibleCoords = drop (length infectedIds) coords

            adefsSusceptible <- mapM (frSIRSNetworkAgent Susceptible) (zip susceptibleCoords susceptibleIds)
            adefsInfected <- mapM (frSIRSNetworkAgent Infected) (zip infectedCoords infectedIds) 

            let env = (createEnvironment
                            Nothing
                            dims
                            moore
                            ClipToMax
                            []
                            rng
                            (Just gr))

            return $ trace ("sortedDegs = " ++ (show $ take numInfected sortedDegs) ++ " infectedIds = " ++ (show infectedIds) ++ " infectedCoords = " ++ (show infectedCoords)) (adefsSusceptible ++ adefsInfected, env)

    where
        agentCount = maxX * maxY
        coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        
        highestDegree :: (AgentId, Int) -> (AgentId, Int) -> Ordering
        highestDegree = (\(_, d1) (_, d2) -> compare d2 d1)

        lowestDegree :: (AgentId, Int) -> (AgentId, Int) -> Ordering
        lowestDegree = (\(_, d1) (_, d2) -> compare d1 d2)
        
frSIRSNetworkAgent :: SIRSState
                        -> (EnvCoord, AgentId)
                        -> IO FrSIRSNetworkAgentDef
frSIRSNetworkAgent initS idCoord = 
    do
        rng <- newStdGen
        let beh = sirsNetworkAgentBehaviour rng initS
        return $ createFrSIRSNetworkDef idCoord initS beh rng

randomFrSIRSNetworkAgent :: Double
                            -> (EnvCoord, AgentId)
                            -> IO FrSIRSNetworkAgentDef
randomFrSIRSNetworkAgent p idCoord = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        
        let isInfected = r <= p
        let initS = if isInfected then Infected else Susceptible

        let beh = sirsNetworkAgentBehaviourRandInfected rng initS

        return $ createFrSIRSNetworkDef idCoord initS beh rng 

createFrSIRSNetworkDef :: (EnvCoord, AgentId) 
                            -> SIRSState 
                            -> FrSIRSNetworkAgentBehaviour
                            -> StdGen 
                            -> FrSIRSNetworkAgentDef
createFrSIRSNetworkDef (coord, agentId) sirsState beh rng = 
    AgentDef { adId = agentId,
                adState = sirsState,
                adBeh = beh,
                adInitMessages = NoEvent,
                adConversation = Nothing,
                adEnvPos = coord,
                adRng = rng }
------------------------------------------------------------------------------------------------------------------------