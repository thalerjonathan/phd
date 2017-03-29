{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

-- TODO Implement SugarScape Chapters
    -- TODO environment also needs a signalfunction which runs once per simulation-step: environmentIn/out
    -- TODO test creation / killing of agents
    -- TODO random iteration in sequential

-- TODO implement rules as SF which can be turned on or off
-- TODO formalize rules in my EDSL
-- TODO problem of sugarscape trading in our functional approach: cannot reply immediately thus potentially violating budget constraints. need to solve this e.g. by having a temporary reserved amount "open for transaction"

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type SugarScapeMsg = ()

data SugarScapeAgentState = SugarScapeAgentState {
    sugAgMetabolism :: Double,
    sugAgVision :: Int,
    sugAgSugar :: Double,
    sugAgRng :: StdGen
} deriving (Show)

data SugarScapeEnvCell = SugarScapeEnvCell {
    sugEnvSugarCapacity :: Double,
    sugEnvSugarLevel :: Double,
    sugEnvOccupied :: Maybe AgentId
} deriving (Show)

type SugarScapeEnvironment = Environment SugarScapeEnvCell
type SugarScapeEnvironmentBehaviour = EnvironmentBehaviour SugarScapeEnvCell

type SugarScapeAgentDef = AgentDef SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentBehaviour = AgentBehaviour SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentIn = AgentIn SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentOut = AgentOut SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
sugarGrowbackRate :: Double
sugarGrowbackRate = 2.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (1.0, 10.0)

sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = (5.0, 10.0)

metabolismRange :: (Double, Double)
metabolismRange = (1.0, 4.0)

visionRange :: (Int, Int)
visionRange = (1, 6)
------------------------------------------------------------------------------------------------------------------------

cellOccupied :: SugarScapeEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupied cell

sugarScapeEnvironmentBehaviour :: SugarScapeEnvironmentBehaviour
sugarScapeEnvironmentBehaviour env = regrowSugarEnv
    where
        regrowSugarEnv = updateEnvironmentCells
                            env
                            (\c -> c {
                                sugEnvSugarLevel =
                                    min
                                        (sugEnvSugarCapacity c)
                                        ((sugEnvSugarLevel c) + sugarGrowbackRate)
                                        } )

agentAction :: SugarScapeAgentOut -> SugarScapeAgentOut
agentAction a
    | starvedToDeath agentAfterMove = trace ("Agent " ++ (show $ aoId a) ++ " starved to death") (agentAfterMove)
    | otherwise = trace ("Agent " ++ (show $ aoId a) ++ " has sugarlevel of " ++ (show $ sugAgSugar $ aoState agentAfterMove)) agentAfterMove
    where
        agentAfterMove = agentMetabolism $ agentCollecting a

starvedToDeath :: SugarScapeAgentOut -> Bool
starvedToDeath a = sugAgSugar s <= 0
    where
        s = aoState a

agentMetabolism :: SugarScapeAgentOut -> SugarScapeAgentOut
agentMetabolism a = updateState
                            a
                            (\s -> s {
                                sugAgSugar =
                                    max
                                        0
                                        ((sugAgSugar s) - (sugAgMetabolism s))})

agentCollecting :: SugarScapeAgentOut -> SugarScapeAgentOut
agentCollecting a
    | null unoccupiedCells = a      -- NOTE: no free cells to move to
    | otherwise = aMoved
    where
        cellsInSight = agentLookout a
        unoccupiedCells = trace ("Agent " ++ (show $ aoId a) ++ " looked out: " ++ (show cellsInSight)) (filter (not . cellOccupied . snd) cellsInSight)

        cellsSortedBySugarLevel = sortBy (\c1 c2 -> compare (sugEnvSugarLevel $ snd c1) (sugEnvSugarLevel $ snd c2)) unoccupiedCells
        bestSugarLevel = sugEnvSugarLevel $ snd $ head cellsSortedBySugarLevel
        bestSugarCells = filter ((==bestSugarLevel) . sugEnvSugarLevel . snd) cellsSortedBySugarLevel

        -- TODO: pick best distance
        (a', (targetCoord, targetCell)) = agentPickRandom a bestSugarCells

        aCollected = updateState a' (\s -> s { sugAgSugar = sugEnvSugarLevel targetCell })

        targetCellOccupied = targetCell { sugEnvSugarLevel = 0.0, sugEnvOccupied = Just (aoId a) }

        env = aoEnv a

        oldCellCoord = aoEnvPos a
        oldCell = cellAt env oldCellCoord
        oldCellUnoccupied = oldCell { sugEnvOccupied = Nothing }

        env' = changeCellAt env oldCellCoord oldCellUnoccupied
        env'' = changeCellAt env targetCoord targetCellOccupied

        aMoved = aCollected { aoEnvPos = targetCoord, aoEnv = env'' }

agentPickRandom :: SugarScapeAgentOut -> [(EnvCoord, SugarScapeEnvCell)] -> (SugarScapeAgentOut, (EnvCoord, SugarScapeEnvCell))
agentPickRandom a allCells@(c:cs)
    | null cs = (a, c)
    | otherwise = (a', randCell)
    where
        g = sugAgRng $ aoState a
        cellCount = length allCells
        (randIdx, g') = randomR (0, cellCount - 1) g
        randCell = allCells !! randIdx
        a' = updateState a (\s -> s { sugAgRng = g' } )

agentLookout :: SugarScapeAgentOut -> [(EnvCoord, SugarScapeEnvCell)]
agentLookout a = zip visionCoordsWrapped visionCells
    where
        env = aoEnv a
        aPos = aoEnvPos a
        n = envNeighbourhood env
        vis = sugAgVision $ aoState a
        visionCoordsDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. vis]
        visionCoords = neighbourhoodOf aPos visionCoordsDeltas
        visionCoordsWrapped = wrapCells (envLimits env) (envWrapping env) visionCoords
        visionCells = cellsAt env visionCoordsWrapped

sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< agentAction aout