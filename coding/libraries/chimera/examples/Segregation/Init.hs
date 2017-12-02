module Segregation.Init (
    createSegregation
  ) where

import Segregation.Model
import Segregation.Agent

import FRP.FrABS

import FRP.Yampa

import System.Random

createSegregation :: Discrete2dDimension -> IO ([SegAgentDef], SegEnvironment)
createSegregation dims@(x,y) =  
    do
        let coords = [ (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
        (adefs, cells) <- populateEnv coords
        rng <- newStdGen

        let e = createDiscrete2d
                            dims
                            moore
                            WrapBoth
                            cells
                            rng

        return (adefs, e)

populateEnv :: [Discrete2dCoord] -> IO ([SegAgentDef], [(Discrete2dCoord, SegEnvCell)])
populateEnv coords = foldr populateEnvAux (return ([], [])) coords

populateEnvAux :: Discrete2dCoord
                    -> IO ([SegAgentDef], [(Discrete2dCoord, SegEnvCell)])
                    -> IO ([SegAgentDef], [(Discrete2dCoord, SegEnvCell)])
populateEnvAux coord accIO = 
    do
        (accAs, accCells) <- accIO

        let agentId = length accAs

        s <- randomAgentState coord
        rng <- newStdGen

        let a = AgentDef { adId = agentId,
                            adState = s,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adBeh = segAgentBehaviour,
                            adRng = rng }

        let emptyCell = (coord, Nothing)
        let occupiedCell = (coord, Just (segParty s))

        r <- getStdRandom (randomR(0.0, 1.0))
        if r < density then
            return ((a : accAs), occupiedCell : accCells)
            else
                return (accAs, emptyCell : accCells)

randomAgentState :: Discrete2dCoord -> IO SegAgentState
randomAgentState coord = 
    do
        r <- getStdRandom (randomR(0.0, 1.0))
        let isRed = (r <= redGreenDist)

        let s = if isRed then
                    Red
                    else
                        Green

        return SegAgentState {
                segParty = s,
                segSimilarityWanted = similarityWanted,
                segSatisfactionLevel = 0.0,
                segCoord = coord }