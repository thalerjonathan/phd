module FrSIRSSpatial.Init (
    createFrSIRSSpatialRandomInfected,
    createFrSIRSSpatialSingleInfected
  ) where

import FrSIRSSpatial.Model
import FrSIRSSpatial.Agent

import FRP.Yampa

import FRP.FrABS

import System.Random

createFrSIRSSpatialRandomInfected :: (Int, Int) 
                                        -> Double 
                                        -> IO ([FrSIRSSpatialAgentDef], FrSIRSSpatialEnvironment)
createFrSIRSSpatialRandomInfected dims@(maxX, maxY) p =  
    do
        rng <- newStdGen

        let agentCount = maxX * maxY
        let aids = [0 .. agentCount - 1]
        let coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        let cells = zip coords aids

        adefs <- mapM (randomFrSIRSAgent p) cells

        let env = createEnvironment
                        Nothing
                        dims
                        neumann -- moore
                        ClipToMax
                        cells
                        rng
                        Nothing

        return (adefs, env)

createFrSIRSSpatialSingleInfected :: (Int, Int) -> IO ([FrSIRSSpatialAgentDef], FrSIRSSpatialEnvironment)
createFrSIRSSpatialSingleInfected dims@(maxX, maxY) =  
    do
        rng <- newStdGen

        let agentCount = maxX * maxY
        let aids = [0 .. agentCount - 1]
        let coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        let cells = zip coords aids

        let centerX = floor $ fromIntegral maxX * 0.5
        let centerY = floor $ fromIntegral maxY * 0.5

        adefs <- mapM (susceptibleFrSIRSAgent (centerX, centerY)) cells

        let env = createEnvironment
                        Nothing
                        dims
                        neumann -- moore
                        ClipToMax
                        cells
                        rng
                        Nothing

        return (adefs, env)

susceptibleFrSIRSAgent :: (Int, Int)
                            -> (EnvCoord, AgentId)
                            -> IO FrSIRSSpatialAgentDef
susceptibleFrSIRSAgent center (pos, agentId) = 
    do
        rng <- newStdGen

        let state = if center == pos then Infected else Susceptible

        return AgentDef { adId = agentId,
                            adState = state,
                            adBeh = sirsAgentBehaviour rng state,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = pos,
                            adRng = rng }

randomFrSIRSAgent :: Double
                    -> (EnvCoord, AgentId)
                    -> IO FrSIRSSpatialAgentDef
randomFrSIRSAgent p (pos, agentId) = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        
        let isInfected = r <= p
        let initS = if isInfected then Infected else Susceptible

        return AgentDef { adId = agentId,
                            adState = initS,
                            adBeh = sirsAgentBehaviourRandInfected rng initS,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = pos,
                            adRng = rng }
------------------------------------------------------------------------------------------------------------------------