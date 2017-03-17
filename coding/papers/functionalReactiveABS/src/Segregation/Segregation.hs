{-# LANGUAGE Arrows #-}
module Segregation.Segregation where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import System.IO
import System.Random

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SegAgentType = Red | Green deriving (Eq, Show)
type SegMsg = ()    -- Agents are not communicating in Schelling Segregation

type SegCoord = (Int, Int)

data SegAgentState = SegAgentState {
    segAgentType :: SegAgentType,
    segCoord :: SegCoord,
    segRng :: StdGen
} deriving (Show)

type SegEnvCell = Maybe SegAgentType
type SegEnvironment = Environment SegEnvCell

type SegAgentDef = AgentDef SegAgentState SegMsg SegEnvCell
type SegAgentBehaviour = AgentBehaviour SegAgentState SegMsg SegEnvCell
type SegAgentOut = AgentOut SegAgentState SegMsg SegEnvCell
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

redGreenDist :: Double
redGreenDist = 0.5
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


------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
------------------------------------------------------------------------------------------------------------------------
createSegAgentsAndEnv :: (Int, Int) -> IO ([SegAgentDef], SegEnvironment)
createSegAgentsAndEnv limits@(x,y) =  do
                                        let coords = [ (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                        (asDefs, envCells) <- populateEnv limits coords
                                        let env = createEnvironment
                                                              Nothing
                                                              limits
                                                              moore
                                                              WrapBoth
                                                              envCells

                                        let as = map (\s -> createAgent s limits) asDefs
                                        return (as, env)
    where
        populateEnv :: (Int, Int) -> [(Int, Int)] -> IO ([SegAgentState], [(EnvCoord, SegEnvCell)])
        populateEnv max coords = foldr (populateEnvAux max) (return ([], [])) coords

            where
                populateEnvAux :: (Int, Int)
                                    -> (Int, Int)
                                    -> IO ([SegAgentState], [(EnvCoord, SegEnvCell)])
                                    -> IO ([SegAgentState], [(EnvCoord, SegEnvCell)])
                populateEnvAux max coord accIO = do
                                                    (accAs, accCells) <- accIO

                                                    ra <- randomAgentState max coord

                                                    let emptyCell = (coord, Nothing)
                                                    let occupiedCell = (coord, Just (segAgentType ra))

                                                    r <- getStdRandom (randomR(0.0, 1.0))
                                                    if r < density then
                                                        return ((ra : accAs), occupiedCell : accCells)
                                                        else
                                                            return (accAs, emptyCell : accCells)

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
                                let isRed = (r <= redGreenDist)

                                let s = if isRed then
                                            Red
                                            else
                                                Green

                                rng <- newStdGen

                                return SegAgentState {
                                        segAgentType = s,
                                        segCoord = coord,
                                        segRng = rng }


coordToAid :: (Int, Int) -> SegCoord -> AgentId
coordToAid (xMax, yMax) (x, y) = (y * xMax) + x
------------------------------------------------------------------------------------------------------------------------

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let aoAfterTime = segDt ao 1.0
        returnA -< aoAfterTime