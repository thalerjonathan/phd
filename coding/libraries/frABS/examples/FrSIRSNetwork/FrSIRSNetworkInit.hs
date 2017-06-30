module FrSIRSNetwork.FrSIRSNetworkInit where

import FrSIRSNetwork.FrSIRSNetworkModel
import FrSIRSNetwork.FrSIRSNetworkAgent

import Utils.Utils

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createFrSIRSNetworkFullConnectedRandInfected :: (Int, Int) 
                                                -> Double 
                                                -> IO ([FrSIRSNetworkAgentDef], FrSIRSNetworkEnvironment)
createFrSIRSNetworkFullConnectedRandInfected dims@(maxX, maxY) p =  
    do
        rng <- newStdGen
        adefs <- mapM (randomFrSIRSNetworkAgent p) [0 .. agentCount - 1]

        let env = (createEnvironment
                        Nothing
                        (0,0) -- dimensions of environment do not matter in network
                        moore
                        ClipToMax
                        []
                        rng
                        (Just gr))

        return (adefs, env)

    where
        agentCount = maxX * maxY
        gr = createCompleteGraph agentCount

createFrSIRSNetworkFullConnectedNumInfected :: (Int, Int) 
                                                -> Int 
                                                -> IO ([FrSIRSNetworkAgentDef], FrSIRSNetworkEnvironment)
createFrSIRSNetworkFullConnectedNumInfected dims@(maxX, maxY) numInfected 
    | numInfected > agentCount = error ("Can't create more infections (" ++ (show numInfected) ++ ") than there are agents (" ++ (show agentCount))
    | otherwise =
        do
            rng <- newStdGen
            adefsSusceptible <- mapM (frSIRSNetworkAgent Susceptible) susceptibleIds
            adefsInfected <- mapM (frSIRSNetworkAgent Infected) infectedIds

            let env = (createEnvironment
                            Nothing
                            dims
                            moore
                            ClipToMax
                            []
                            rng
                            (Just gr))

            return (adefsSusceptible ++ adefsInfected, env)

    where
        agentCount = maxX * maxY
        gr = createCompleteGraph agentCount
        susceptibleIds = [0 .. agentCount - numInfected - 1]
        infectedIds = [agentCount - numInfected .. agentCount - 1]
        

frSIRSNetworkAgent :: SIRSState
                        -> AgentId
                        -> IO FrSIRSNetworkAgentDef
frSIRSNetworkAgent initS agentId = 
    do
        rng <- newStdGen
        return $ createFrSIRSNetworkDef agentId initS rng

randomFrSIRSNetworkAgent :: Double
                            -> AgentId
                            -> IO FrSIRSNetworkAgentDef
randomFrSIRSNetworkAgent p agentId = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        
        let isInfected = r <= p
        let initS = if isInfected then Infected else Susceptible

        return $ createFrSIRSNetworkDef agentId initS rng

createFrSIRSNetworkDef :: AgentId 
                            -> SIRSState 
                            -> StdGen 
                            -> FrSIRSNetworkAgentDef
createFrSIRSNetworkDef agentId sirsState rng = 
    AgentDef { adId = agentId,
                adState = sirsState,
                adBeh = (sirsNetworkAgentBehaviour rng sirsState),    -- for testing Yampa-implementation of Agent
                adInitMessages = NoEvent,
                adConversation = Nothing,
                adEnvPos = (0,0), -- dummy position, does not matter in networks
                adRng = rng }
------------------------------------------------------------------------------------------------------------------------