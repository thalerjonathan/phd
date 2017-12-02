module FrSIRSSpatial.Init (
    createFrSIRSSpatialRandomInfected,
    createFrSIRSSpatialSingleInfected
  ) where

import FrSIRSSpatial.Model
import FrSIRSSpatial.Agent

import FRP.Yampa

import FRP.FrABS

import System.Random

createFrSIRSSpatialRandomInfected :: Discrete2dDimension
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

        let e = createDiscrete2d
                        dims
                        neumann -- moore
                        ClipToMax
                        cells
                        rng

        return (adefs, e)

createFrSIRSSpatialSingleInfected :: Discrete2dDimension -> IO ([FrSIRSSpatialAgentDef], FrSIRSSpatialEnvironment)
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

        let e = createDiscrete2d
                        dims
                        moore -- neumann -- moore
                        ClipToMax
                        cells
                        rng

        return (adefs, e)

susceptibleFrSIRSAgent :: Discrete2dCoord
                            -> (Discrete2dCoord, AgentId)
                            -> IO FrSIRSSpatialAgentDef
susceptibleFrSIRSAgent center (coord, agentId) = 
    do
        rng <- newStdGen

        let initS = if center == coord then Infected else Susceptible

        let s = FrSIRSSpatialAgentState {
            sirsState = initS,
            sirsCoord = coord
        }

        return AgentDef { adId = agentId,
                            adState = s,
                            adBeh = sirsAgentBehaviour rng initS,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adRng = rng }

randomFrSIRSAgent :: Double
                    -> (Discrete2dCoord, AgentId)
                    -> IO FrSIRSSpatialAgentDef
randomFrSIRSAgent p (coord, agentId) = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        
        let isInfected = r <= p
        let initS = if isInfected then Infected else Susceptible

        let s = FrSIRSSpatialAgentState {
            sirsState = initS,
            sirsCoord = coord
        }

        return AgentDef { adId = agentId,
                            adState = s,
                            adBeh = sirsAgentBehaviour rng initS,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adRng = rng }
------------------------------------------------------------------------------------------------------------------------