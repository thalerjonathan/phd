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
    -- TODO test creation of agents
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
sugarGrowbackRate = 1.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (1.0, 10.0)

sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = (5.0, 10.0)

metabolismRange :: (Double, Double)
metabolismRange = (1.0, 4.0)

visionRange :: (Int, Int)
visionRange = (1, 1)
------------------------------------------------------------------------------------------------------------------------

cellOccupied :: SugarScapeEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupied cell

cellUnoccupied :: SugarScapeEnvCell -> Bool
cellUnoccupied = not . cellOccupied


sugarScapeEnvironmentBehaviour :: SugarScapeEnvironmentBehaviour
sugarScapeEnvironmentBehaviour env = regrowSugarEnvRate
    where
        regrowSugarEnvRate = updateEnvironmentCells
                            env
                            (\c -> c {
                                sugEnvSugarLevel = (
                                    min
                                        (sugEnvSugarCapacity c)
                                        ((sugEnvSugarLevel c) + sugarGrowbackRate))
                                        } )

        regrowSugarEnvMax = updateEnvironmentCells
                                    env
                                    (\c -> c {
                                        sugEnvSugarLevel = (sugEnvSugarCapacity c)} )

agentAction :: SugarScapeAgentOut -> SugarScapeAgentOut
agentAction a
    | starvedToDeath agentAfterHarvest = kill $ unoccupyPosition agentAfterHarvest
    | otherwise = agentAfterHarvest
    where
        agentAfterHarvest = agentMetabolism $ agentCollecting a

unoccupyPosition ::  SugarScapeAgentOut -> SugarScapeAgentOut
unoccupyPosition a = a { aoEnv = env' }
    where
        env = aoEnv a

        currentAgentPosition = aoEnvPos a
        currentAgentCell = cellAt env currentAgentPosition
        currentAgentCellUnoccupied = currentAgentCell { sugEnvOccupied = Nothing }

        env' = changeCellAt env currentAgentPosition currentAgentCellUnoccupied

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
    | null unoccupiedCells = a -- trace ("Agent " ++ (show $ aoId a) ++ " found no unoccupied cells") a
    | otherwise = aHarvested
    where
        cellsInSight = agentLookout a
        unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight
        bestCells = selectBestCells unoccupiedCells
        (a', cellInfo) = agentPickRandom a bestCells
        aHarvested = agentMoveAndHarvestCell a' cellInfo

agentMoveAndHarvestCell :: SugarScapeAgentOut -> (EnvCoord, SugarScapeEnvCell) -> SugarScapeAgentOut
agentMoveAndHarvestCell a (cellCoord, cell) = updateState a'' (\s -> s { sugAgSugar = newSugarLevelAgent })
    where
        sugarLevelCell = sugEnvSugarLevel cell
        sugarLevelAgent = sugAgSugar $ aoState a
        newSugarLevelAgent = (sugarLevelCell + sugarLevelAgent) -- trace ("Agent " ++ (show $ aoId a) ++ " environment BEFORE move has cells: " ++ (show $ allCellsWithCoords env))

        a' = unoccupyPosition a
        env = aoEnv a'

        cellHarvestedAndOccupied = cell { sugEnvSugarLevel = 0.0, sugEnvOccupied = Just (aoId a) }
        env' = changeCellAt env cellCoord cellHarvestedAndOccupied

        --trace ("Agent " ++ (show $ aoId a) ++ " environment AFTER move has cells: " ++ (show $ allCellsWithCoords env''))
        a'' = a' { aoEnvPos = cellCoord, aoEnv = env' }


selectBestCells :: [(EnvCoord, SugarScapeEnvCell)] -> [(EnvCoord, SugarScapeEnvCell)]
selectBestCells cs = bestSugarCells
    where
        cellsSortedBySugarLevel = sortBy (\c1 c2 -> compare (sugEnvSugarLevel $ snd c2) (sugEnvSugarLevel $ snd c1)) cs
        bestSugarLevel = sugEnvSugarLevel $ snd $ head cellsSortedBySugarLevel
        bestSugarCells = filter ((==bestSugarLevel) . sugEnvSugarLevel . snd) cellsSortedBySugarLevel
        -- TODO: filter by best distance

-- TODO: think about moving this to the general Agent.hs: introduce a Maybe StdGen, but then: don't we loose reasoning abilities?
agentPickRandom :: SugarScapeAgentOut -> [a] -> (SugarScapeAgentOut, a)
agentPickRandom a all@(x:xs)
    | null xs = (a, x)
    | otherwise = (a', randElem)
    where
        g = sugAgRng $ aoState a
        cellCount = length all
        (randIdx, g') = randomR (0, cellCount - 1) g
        randElem = all !! randIdx
        a' = updateState a (\s -> s { sugAgRng = g' } )

agentLookout :: SugarScapeAgentOut -> [(EnvCoord, SugarScapeEnvCell)]
agentLookout a = zip visionCoordsWrapped visionCells
    where
        env = aoEnv a
        aPos = aoEnvPos a
        n = envNeighbourhood env
        vis = sugAgVision $ aoState a

        -- TODO: put this logic into environment.hs
        visionCoordsDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. vis]
        visionCoords = neighbourhoodOf aPos visionCoordsDeltas
        visionCoordsWrapped = wrapCells (envLimits env) (envWrapping env) visionCoords
        visionCells = cellsAt env visionCoordsWrapped

sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< agentAction aout