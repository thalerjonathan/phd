module Segregation.SegregationInit where

import Segregation.SegregationModel

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

import System.Random
import System.IO

createSegAgentsAndEnv :: (Int, Int) -> IO ([SegAgentDef], SegEnvironment)
createSegAgentsAndEnv limits@(x,y) =  do
                                        let coords = [ (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                        (as, envCells) <- populateEnv coords
                                        let env = createEnvironment
                                                              Nothing
                                                              limits
                                                              moore
                                                              WrapBoth
                                                              envCells
                                        return (as, env)
    where
        populateEnv :: [(Int, Int)] -> IO ([SegAgentDef], [(EnvCoord, SegEnvCell)])
        populateEnv coords = foldr populateEnvAux (return ([], [])) coords

        populateEnvAux :: (Int, Int)
                            -> IO ([SegAgentDef], [(EnvCoord, SegEnvCell)])
                            -> IO ([SegAgentDef], [(EnvCoord, SegEnvCell)])
        populateEnvAux coord accIO = do
                                        (accAs, accCells) <- accIO

                                        let agentId = length accAs

                                        s <- randomAgentState
                                        let a = AgentDef { adId = agentId,
                                                            adState = s,
                                                            adEnvPos = coord,
                                                            adInitMessages = NoEvent,
                                                            adConversation = Nothing,
                                                            adBeh = segAgentBehaviour }

                                        let emptyCell = (coord, Nothing)
                                        let occupiedCell = (coord, Just (segParty s))

                                        r <- getStdRandom (randomR(0.0, 1.0))
                                        if r < density then
                                            return ((a : accAs), occupiedCell : accCells)
                                            else
                                                return (accAs, emptyCell : accCells)

        randomAgentState :: IO SegAgentState
        randomAgentState = do
                                r <- getStdRandom (randomR(0.0, 1.0))
                                let isRed = (r <= redGreenDist)

                                let s = if isRed then
                                            Red
                                            else
                                                Green

                                rng <- newStdGen

                                return SegAgentState {
                                        segParty = s,
                                        segSimilarityWanted = similarityWanted,
                                        segSatisfactionLevel = 0.0,
                                        segRng = rng }