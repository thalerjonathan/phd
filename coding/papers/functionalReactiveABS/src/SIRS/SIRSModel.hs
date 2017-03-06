{-# LANGUAGE Arrows #-}

module SIRS.SIRSModel where

import FRP.Yampa
import System.IO
import Debug.Trace
import System.Random
import Agent.Agent

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState

type SIRSCoord = (Int, Int)

data SIRSAgentState = SIRSAgentState {
    sirsState :: SIRSState,
    sirsCoord :: SIRSCoord,
    sirsTime :: Double,
    sirsNeighbours :: [AgentId],
    sirsRng :: StdGen
}

type SIRSAgentDef = AgentDef SIRSAgentState SIRSMsg
type SIRSAgentBehaviour = AgentBehaviour SIRSAgentState SIRSMsg
type SIRSAgentOut = AgentOut SIRSAgentState SIRSMsg
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
infectedDuration :: Double
infectedDuration = 7.0

immuneDuration :: Double
immuneDuration = 3.0

infectionProbability :: Double
infectionProbability = 0.3
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR

sirsAgentBehaviour :: SIRSAgentBehaviour
sirsAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        -- TODO: implement
        returnA -< ao

------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
createRandomSIRSAgents :: (Int, Int) -> Double -> IO [SIRSAgentDef]
createRandomSIRSAgents max@(x,y) p =  do
                                           let ssIO = [ randomAgentState p max (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                           ss <- mapM id ssIO
                                           let as = map (\s -> createAgent s max) ss
                                           return as
    where
        createAgent :: SIRSAgentState -> (Int, Int) -> SIRSAgentDef
        createAgent s max = AgentDef { adId = agentId,
                                        adState = s,
                                        adBehaviour = sirsAgentBehaviour }
            where
                c = sirsCoord s
                agentId = coordToAid max c

randomAgentState :: Double -> (Int, Int) -> SIRSCoord -> IO SIRSAgentState
randomAgentState p max coord = do
                                    r <- getStdRandom (randomR(0.0, 1.0))
                                    let isInfected = r <= p

                                    let s = if isInfected then
                                                Infected
                                                else
                                                    Susceptible

                                    let nCoords = neighbours coord max
                                    let nIds = map (coordToAid max) nCoords
                                    rng <- newStdGen

                                    return SIRSAgentState{
                                            sirsState = s,
                                            sirsCoord = coord,
                                            sirsTime = 0.0,
                                            sirsNeighbours = nIds,
                                            sirsRng = rng }


coordToAid :: (Int, Int) -> SIRSCoord -> AgentId
coordToAid (xMax, yMax) (x, y) = (y * xMax) + x

neighbours :: SIRSCoord -> (Int, Int) -> [SIRSCoord]
neighbours (x,y) max = clipCoords allCoords max
    where
        allCoords = map (\(x', y') -> (x+x', y+y')) neighbourhood

clipCoords :: [SIRSCoord] -> (Int, Int) -> [SIRSCoord]
clipCoords cs max = filter (\c -> validCoord c max ) cs
    where
        validCoord :: SIRSCoord -> (Int, Int) -> Bool
        validCoord (x, y) (xMax, yMax)
            | x < 0 = False
            | y < 0 = False
            | x >= xMax = False
            | y >= yMax = False
            | otherwise = True

neighbourhood :: [(Int, Int)]
neighbourhood = [topLeft, top, topRight,
                 left, right,
                 bottomLeft, bottom, bottomRight]
    where
        topLeft =       (-1, -1)
        top =           ( 0, -1)
        topRight =      ( 1, -1)
        left =          (-1,  0)
        right =         ( 1,  0)
        bottomLeft =    (-1,  1)
        bottom =        ( 0,  1)
        bottomRight =   ( 1,  1)
------------------------------------------------------------------------------------------------------------------------