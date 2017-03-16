{-# LANGUAGE Arrows #-}

module Segregation.Segregation where

import FRP.Yampa
import System.IO
import Debug.Trace
import System.Random
import FrABS.Agent.Agent

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SegAgentType = Red | Green deriving (Eq, Show)
type SegMsg = ()

type SegCoord = (Int, Int)

data SegAgentState = SegAgentState {
    segAgentType :: SegAgentType,
    segCoord :: SegCoord,
    segRng :: StdGen
} deriving (Show)

type SegAgentDef = AgentDef SegAgentState SegMsg
type SegAgentBehaviour = AgentBehaviour SegAgentState SegMsg
type SegAgentOut = AgentOut SegAgentState SegMsg
------------------------------------------------------------------------------------------------------------------------

-- TODO implement segregation
-- TODO implement recursive simulations: MetaABS: test it using the segregation model. run recursive simulation for local neighbourhood only
    -- for each move compute 1 step recursive and see how this move performs. if it improves the outcome, then take it otherwise stay.

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
acceptPercent :: Double
acceptPercent = 0.3

density :: Double
density = 0.5

redBlueDist :: Double
redBlueDist = 0.5
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
is :: SegAgentOut -> SegAgentType -> Bool
is ao sat = (segAgentType s) == sat
    where
        s = aoState ao

segDt :: SegAgentOut -> Double -> SegAgentOut
segDt ao dt = ao

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let aoAfterTime = segDt ao 1.0
        returnA -< aoAfterTime
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
------------------------------------------------------------------------------------------------------------------------
createRandomSegAgents :: (Int, Int) -> IO [SegAgentDef]
createRandomSegAgents max@(x,y) =  do
                                       let ssIO = [ randomAgentState max (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                       ss <- mapM id ssIO
                                       let as = map (\s -> createAgent s max) ss
                                       return as
    where
        createAgent :: SegAgentState -> (Int, Int) -> SegAgentDef
        createAgent s max = AgentDef { adId = agentId,
                                        adState = s,
                                        adBehaviour = segAgentBehaviour }
            where
                c = segCoord s
                agentId = coordToAid max c

randomAgentState :: (Int, Int) -> SegCoord -> IO SegAgentState
randomAgentState max coord = do
                                r <- getStdRandom (randomR(0.0, 1.0))
                                let isRed = (r <= redBlueDist)

                                let s = if isRed then
                                            Red
                                            else
                                                Green

                                let nCoords = neighbours coord max
                                let nIds = map (coordToAid max) nCoords
                                rng <- newStdGen

                                return SegAgentState {
                                        segAgentType = s,
                                        segCoord = coord,
                                        segRng = rng }


coordToAid :: (Int, Int) -> SegCoord -> AgentId
coordToAid (xMax, yMax) (x, y) = (y * xMax) + x

neighbours :: SegCoord -> (Int, Int) -> [SegCoord]
neighbours (x,y) max = clipCoords allCoords max
    where
        allCoords = map (\(x', y') -> (x+x', y+y')) neighbourhood

clipCoords :: [SegCoord] -> (Int, Int) -> [SegCoord]
clipCoords cs max = filter (\c -> validCoord c max ) cs
    where
        validCoord :: SegCoord -> (Int, Int) -> Bool
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