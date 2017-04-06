{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeAgent where

-- Project-internal import first
import SugarScape.SugarScapeModel
import SugarScape.SugarScapeEnvironment
import FrABS.Env.Environment
import FrABS.Agent.Agent

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List
import System.Random

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentDies :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDies = unoccupyPosition . kill

agentAction :: SugarScapeAgentOut -> SugarScapeAgentOut
agentAction a
    | starvedToDeath agentAfterHarvest = agentDies agentAfterHarvest
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
starvedToDeath a = sugAgSugarLevel s <= 0
    where
        s = aoState a

agentMetabolism :: SugarScapeAgentOut -> SugarScapeAgentOut
agentMetabolism a = updateState
                            a
                            (\s -> s {
                                sugAgSugarLevel =
                                    max
                                        0
                                        ((sugAgSugarLevel s) - (sugAgMetabolism s))})

agentCollecting :: SugarScapeAgentOut -> SugarScapeAgentOut
agentCollecting a
    | null unoccupiedCells = a
    | otherwise = aHarvested
    where
        cellsInSight = agentLookout a
        unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

        bestCells = selectBestCells (aoEnvPos a) unoccupiedCells
        -- NOTE: can return equally good cells, do random selection
        (a', cellInfo) = agentPickRandom a bestCells

        aHarvested = agentMoveAndHarvestCell a' cellInfo

agentMoveAndHarvestCell :: SugarScapeAgentOut -> (EnvCoord, SugarScapeEnvCell) -> SugarScapeAgentOut
agentMoveAndHarvestCell a (cellCoord, cell) = updateState a'' (\s -> s { sugAgSugarLevel = newSugarLevelAgent })
    where
        sugarLevelCell = sugEnvSugarLevel cell
        sugarLevelAgent = sugAgSugarLevel $ aoState a
        newSugarLevelAgent = (sugarLevelCell + sugarLevelAgent)

        a' = unoccupyPosition a
        env = aoEnv a'

        agentMetabolism = sugAgMetabolism $ aoState a
        polutionIncByMeta =  agentMetabolism * polutionMetabolismFactor
        polutionIncByHarvest = sugarLevelCell * polutionHarvestFactor
        -- newPolutionLevel = polutionIncByMeta + polutionIncByHarvest + sugEnvPolutionLevel cell
        newPolutionLevel = 0

        cellHarvestedAndOccupied = cell {
                sugEnvSugarLevel = 0.0,
                sugEnvOccupied = Just (aoId a),
                sugEnvPolutionLevel = newPolutionLevel
                }
        env' = changeCellAt env cellCoord cellHarvestedAndOccupied

        a'' = a' { aoEnvPos = cellCoord, aoEnv = env' }


selectBestCells :: EnvCoord -> [(EnvCoord, SugarScapeEnvCell)] -> [(EnvCoord, SugarScapeEnvCell)]
selectBestCells refCoord cs = bestShortestDistanceCells
    where
        measureFunc = bestMeasureSugarLevel

        cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
        bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
        bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

        shortestDistanceBestCells = sortBy (\c1 c2 -> compare (distance refCoord (fst c1)) (distance refCoord (fst c2))) bestCells
        shortestDistance = distance refCoord (fst $ head shortestDistanceBestCells)
        bestShortestDistanceCells = filter ((==shortestDistance) . (distance refCoord) . fst) shortestDistanceBestCells

        bestMeasureSugarLevel :: SugarScapeEnvCell -> Double
        bestMeasureSugarLevel c = sugEnvSugarLevel c

        bestMeasureSugarPolutionRatio :: SugarScapeEnvCell -> Double
        bestMeasureSugarPolutionRatio c = sugLvl / (1 + polLvl)
            where
                sugLvl = sugEnvSugarLevel c
                polLvl = sugEnvPolutionLevel c

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

agentAgeing :: SugarScapeAgentOut -> Double -> SugarScapeAgentOut
agentAgeing a age
    | dieFromAge a age = agentDies $ birthNewAgent a
    | otherwise = agentAction a

birthNewAgent :: SugarScapeAgentOut -> SugarScapeAgentOut
birthNewAgent a = createAgent a newAgentDef
    where
        newAgentId = aoId a                                 -- NOTE: we keep the old id
        (newAgentCoord, a') = findUnoccpiedRandomPosition a
        oldAgentRng = sugAgRng $ aoState a'
        (newAgentDef, _) = randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviour oldAgentRng

        findUnoccpiedRandomPosition :: SugarScapeAgentOut -> (EnvCoord, SugarScapeAgentOut)
        findUnoccpiedRandomPosition a
            | cellOccupied c = findUnoccpiedRandomPosition a'
            | otherwise = (coord, a')
            where
                g = sugAgRng $ aoState a
                env = aoEnv a
                (c, coord, g') = randomCell g env
                a' = updateState a (\s -> s { sugAgRng = g' })

dieFromAge :: SugarScapeAgentOut -> Double -> Bool
dieFromAge a age = age >= (sugAgMaxAge $ aoState a)
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSex :: Double -> SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentSex age ain a
    | isFertile a age &&
        satisfiesWealthForChildBearing a &&
        hasMatingNeighbours &&
        hasSpaceForOffspring = (agentHandleMatingRequests ain) $ agentSendMatingRequests occupiedIds a
    | otherwise = a  -- NOTE: this will ignore all Mating Messages and implicitly discharges it
    where
        ncs = getNeighbourCells a
        occupiedCells = filter (isJust . sugEnvOccupied . snd) ncs
        unoccupiedCells = filter (isNothing . sugEnvOccupied . snd) ncs
        occupiedIds = map (fromJust . sugEnvOccupied . snd) occupiedCells
        hasMatingNeighbours = (not . null) occupiedIds
        hasSpaceForOffspring = (not . null) unoccupiedCells

agentSendMatingRequests :: [AgentId] -> SugarScapeAgentOut -> SugarScapeAgentOut
agentSendMatingRequests ais a = sendMessages a msgs
    where
        s = aoState a
        m =  Mating {
                    matingMsgGender = sugAgGender s,
                    matingMsgMetab = sugAgMetabolism s,
                    matingMsgVision = sugAgVision s,
                    matingMsgSugar = sugAgSugarLevel s
                }
        msgs = zip ais (replicate (length ais) m)

agentHandleMatingRequests :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentHandleMatingRequests ain a = onMessage
                                    matingRequest
                                    ain
                                    (\a' (senderId, m) -> trace ("Agent " ++ (show $ aoId a) ++ " received Mating from " ++ show (senderId) ++ ": " ++ show m) a')
                                    a

    where
        matingRequest :: AgentMessage SugarScapeMsg -> Bool
        matingRequest (_, Mating a b c d) = True
        -- matingRequest otherwise = False

satisfiesWealthForChildBearing :: SugarScapeAgentOut -> Bool
satisfiesWealthForChildBearing a = currSugar >= initSugar
    where
        currSugar = sugAgSugarLevel $ aoState a
        initSugar = sugAgSugarInit $ aoState a

isFertile :: SugarScapeAgentOut -> Double -> Bool
isFertile a age = withinRange age fertilityAgeRange
    where
        fertilityAgeRange = sugAgFertAgeRange $ aoState a

withinRange :: (Ord a) => a -> (a, a) -> Bool
withinRange a (l, u) = a >= l && a <= u

getNeighbourCells :: SugarScapeAgentOut -> [(EnvCoord, SugarScapeEnvCell)]
getNeighbourCells a = neighbours env pos
    where
        env = aoEnv a
        pos = aoEnvPos a

getNeighbours :: SugarScapeAgentOut -> [AgentId]
getNeighbours a = nids
    where
        env = aoEnv a
        pos = aoEnvPos a
        ns = neighbours env pos
        nids = foldr (\(_, c) acc -> if isJust $ sugEnvOccupied c then (fromJust $ sugEnvOccupied c) : acc else acc ) [] ns
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- GENERAL AGENT-RELATED
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = proc ain ->
    do
        age <- time -< 0

        let a = agentOutFromIn ain

        let a0 = agentAgeing a age
        let a1 = if isKilled a0 then a0 else agentSex age ain a0

        returnA -< a1
------------------------------------------------------------------------------------------------------------------------