{-# LANGUAGE Arrows #-}
module Segregation.SegregationAgent where

import Segregation.SegregationModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentRandom
import FrABS.Env.Environment

import Data.Maybe



------------------------------------------------------------------------------------------------------------------------
-- HELPER-Functions
------------------------------------------------------------------------------------------------------------------------
isParty :: SegAgentOut -> SegParty -> Bool
isParty ao p = (segParty s) == p
    where
        s = aoState ao

isOccupied :: SegEnvCell -> Bool
isOccupied = isJust

isSatisfied :: SegAgentOut -> Bool
isSatisfied aout = (segSatisfactionLevel s) >= (segSimilarityWanted s)
    where
        s = aoState aout
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
-- the recursive case: only move to the target point when it satisfies ourself in the present and in the future
    -- satisfied in the present: stay & check if satisfied in future
            -- satisfied in future: stay
            -- NOT satsfied in future: move

    -- not satisfied in the present: move and don't care about future

-- in the recursive case we have the SegAgentOut of the present which initiated the recursion AND the SegAgentOut of the future
-- in the recursive case (future) we need to decide the above cases and return the corresponding SegAgentOut
-- IMPORTANT: we always need to return the ENVIRONMENT of the PRESENT SegAgentOut
segMovementRec :: SegAgentIn -> SegAgentOut -> SegAgentOut
segMovementRec ain ao 
    -- trace ("Agent " ++ (show $ aoId ao) ++ " isRecursive") 
    | isRecursive ain = (unrecursive $ segMovementFuture ain ao)         -- NOTE: ALWAYS unrecursive, only 1 recursion
    | otherwise = segMovementPresent ain ao   
    where
        -- NOTE: we know that the present IS SATISFIED given is environment and that is HAS NOT MOVED
        segMovementFuture :: SegAgentIn -> SegAgentOut -> SegAgentOut    
        segMovementFuture ain ao
            -- trace ("Agent " ++ (show $ aoId ao) ++ " is satisfied in future, stay in present") 
            | isSatisfied aoFuture = aoPresent      -- satisfied in the future as well, stay at same position as present
            -- trace ("Agent " ++ (show $ aoId ao) ++ " is not satisfied in future, move in present") 
            | otherwise = (move aoPresent)            -- unhappy in the future, move in the present
            where
                -- NOTE: we need the present one with the present environment. although ao has the same state it has NOT the same environment
                aoPresent = head $ fromEvent $ aiRec ain
                -- NOTE: the aoPresent becomes the future one by calculating the satisfaction-level which depends on the environment which was changed
                -- by the other agents which were simulated before (recursive simulation!)
                aoFuture = updateSatisfactionLevel ao

        segMovementPresent :: SegAgentIn -> SegAgentOut -> SegAgentOut    
        segMovementPresent ain ao
            -- trace ("Agent " ++ (show $ aoId ao) ++ " satisfied in present, start recursion") 
            -- NOTE: to reduce the ratio of agents using future-forecasting to 50% instead of all 100% use: && (mod (aoId ao) 2 == 0)
            | isSatisfied ao' 
                && recInitAllowed ain
                = (recursive ao' False)     -- satisfied in the present: check for future, otherwise don't care for future and just move
            | otherwise = ao'
            where
                -- NOTE: this is the decision in the present (non-recursive)
                ao' = (segMovement ao ) -- trace ("Agent " ++ (show $ aoId ao) ++ " makes move, recAllowed: " ++ (show $ recInitAllowed ain)) 

segMovement :: SegAgentOut -> SegAgentOut
segMovement ao 
    | isSatisfied ao' = ao'
    | otherwise = move ao'
    where
        -- NOTE: need to re-calculate similarity and update agent because could have changed since last update
        ao' = updateSatisfactionLevel ao

updateSatisfactionLevel :: SegAgentOut -> SegAgentOut
updateSatisfactionLevel ao = updateDomainState ao (\s -> s { segSatisfactionLevel = updatedSatisfactionLevel } )
    where
        agentPos = aoEnvPos ao
        updatedSatisfactionLevel = satisfactionOn ao agentPos

-- NOTE: this function calculates the projected satisfaction of the party if the given coordinate is occupied by this party
satisfactionOn :: SegAgentOut -> EnvCoord -> Double
satisfactionOn ao targetCoord
    | Red == party = redFraction
    | otherwise = greenFraction
    where
        env = aoEnv ao
        party = segParty $ aoState ao
        cs = neighbours env targetCoord
        (reds, greens) = countOccupied cs

        -- NOTE: if the agent is in the neighbourhood then don't include itself, otherwise include itself (although it might be on the targetcoord, the targetCoord itself is not included)
        includeAgent = if isAgentInNeighbourhood ao cs then 0 else 1
        totalCount = (fromInteger $ fromIntegral (reds + greens + includeAgent))
        redFraction = (fromInteger $ fromIntegral (reds + includeAgent)) / totalCount
        greenFraction = (fromInteger $ fromIntegral (greens + includeAgent)) / totalCount

        isAgentInNeighbourhood :: SegAgentOut -> [(EnvCoord, SegEnvCell)] -> Bool
        isAgentInNeighbourhood ao cs = any ((==(agentPos)) . fst) cs
            where
                agentPos = aoEnvPos ao

        countOccupied :: [(EnvCoord, SegEnvCell)] -> (Int, Int)
        countOccupied cs = (redCount, greenCount)
            where
                occupiedCells = filter (isOccupied . snd) cs
                redCount = length $ filter ((==Red) . fromJust . snd) occupiedCells
                greenCount = length $ filter ((==Green) . fromJust . snd) occupiedCells


move :: SegAgentOut -> SegAgentOut
move ao = maybe ao' (\optCoord -> moveTo ao' optCoord) mayOptCoord
    where
        (ao', mayOptCoord) = findMove
                                optimizingStrategy
                                movementStrategy
                                selectionStrategy
                                ao

moveTo :: SegAgentOut -> EnvCoord -> SegAgentOut
moveTo ao newCoord = ao'
    where
        s = aoState ao
        env = aoEnv ao
        oldCoord = aoEnvPos ao
        c = segParty s
        env' = changeCellAt env oldCoord Nothing
        env'' = changeCellAt env' newCoord (Just c)
        ao' = ao { aoEnv = env'', aoEnvPos = newCoord }

findMove :: SegOptStrategy
                -> SegMoveStrategy
                -> SegSelectionStrategy
                -> SegAgentOut
                -> (SegAgentOut, Maybe EnvCoord)
findMove opts ms SelectNearest ao = (ao,
                                        findNearestFreeCoord
                                        ao
                                        (optimizeDistance opts ao)
                                        ms)
findMove opts ms (SelectRandom optRetries freeCellRetries) ao = findOptRandomFreeCoord
                                                                    ao
                                                                    ms
                                                                    optRetries
                                                                    freeCellRetries
                                                                    (optimizeSatisfaction opts)
------------------------------------------------------------------------------------------------------------------------
-- OPTIMIZING
------------------------------------------------------------------------------------------------------------------------
moveAlways :: SegAgentOut -> EnvCoord -> Bool
moveAlways _ _ = True

moveSatisfies :: SegAgentOut -> EnvCoord -> Bool
moveSatisfies ao targetCoord = similiarityOnCoord > (segSimilarityWanted (aoState ao))
    where
        similiarityOnCoord = satisfactionOn ao targetCoord

moveImproves :: SegAgentOut -> EnvCoord -> Bool
moveImproves ao targetCoord = satisfactionLevelOnCoord > satisfactionLevel
    where
        satisfactionLevelOnCoord = satisfactionOn ao targetCoord
        satisfactionLevel = (segSatisfactionLevel (aoState ao))

optimizeSatisfaction :: SegOptStrategy -> SegAgentOut -> EnvCoord -> Bool
optimizeSatisfaction OptNone = moveAlways
optimizeSatisfaction OptSimilaritySatisfied = moveSatisfies
optimizeSatisfaction OptSimilarityIncreasing = moveImproves

optimizeDistance :: SegOptStrategy
                        -> SegAgentOut
                        -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell)
optimizeDistance opts ao best@(bestCoord, _) comp@(compCoord, _) = if distComp < distBest then
                                                                                     if improves then
                                                                                        comp
                                                                                        else
                                                                                            best
                                                                                     else
                                                                                         best

    where
        agentPos = aoEnvPos ao
        improves = optimizeSatisfaction opts ao compCoord
        distBest = distanceManhattan agentPos bestCoord
        distComp = distanceManhattan agentPos compCoord
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Nearest Cell Selection
------------------------------------------------------------------------------------------------------------------------
findNearestFreeCoord :: SegAgentOut
                        -> ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                        -> SegMoveStrategy
                        -> Maybe EnvCoord
findNearestFreeCoord ao optFunc strat = maybe Nothing (Just . fst) mayNearest
    where
        env = aoEnv ao
        coord = aoEnvPos ao
        
        coordsCellsSearch = case strat of   (MoveLocal r) -> cellsAroundRect env coord r
                                            MoveGlobal -> allCellsWithCoords env

        mayNearest = foldr (findNearestAux optFunc) Nothing coordsCellsSearch

findNearestAux :: ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                    -> (EnvCoord, SegEnvCell)
                    -> Maybe (EnvCoord, SegEnvCell)
                    -> Maybe (EnvCoord, SegEnvCell)
findNearestAux _ comp@(_, compCell) Nothing
    | isOccupied compCell = Nothing
    | otherwise = Just comp
findNearestAux optFunc comp@(_, compCell) best@(Just (bestCoord, bestCell))
    | isOccupied compCell = best
    | otherwise = Just $ optFunc (bestCoord, bestCell) comp
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Random Cell Selection
------------------------------------------------------------------------------------------------------------------------
findOptRandomFreeCoord :: SegAgentOut
                            -> SegMoveStrategy
                            -> Int
                            -> Int
                            -> (SegAgentOut -> EnvCoord -> Bool)
                             -> (SegAgentOut, Maybe EnvCoord)
findOptRandomFreeCoord ao ms 0 freeCellRetries _ = findRandomFreeCoord ao ms freeCellRetries
findOptRandomFreeCoord ao ms optRetries freeCellRetries optFunc =
        maybe ret (\freeCoord -> if optFunc ao' freeCoord then
                                    ret
                                    else
                                        findOptRandomFreeCoord ao' ms (optRetries - 1) freeCellRetries optFunc) mayFreeCoord
    where
        ret@(ao', mayFreeCoord) = findRandomFreeCoord ao ms freeCellRetries

findRandomFreeCoord :: SegAgentOut -> SegMoveStrategy -> Int -> (SegAgentOut, Maybe EnvCoord)
findRandomFreeCoord ao _ 0 = (ao, Nothing)
findRandomFreeCoord ao ms maxRetries
    | isOccupied randCell = findRandomFreeCoord ao' ms (maxRetries - 1)
    | otherwise = (ao', Just randCoord)
    where
        (ao', randCell, randCoord) = findRandomFreeCoordAux ms ao

        findRandomFreeCoordAux :: SegMoveStrategy -> SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
        findRandomFreeCoordAux (MoveLocal radius) ao = localRandomCell ao radius
        findRandomFreeCoordAux MoveGlobal ao = globalRandomCell ao

localRandomCell :: SegAgentOut -> Int -> (SegAgentOut, SegEnvCell, EnvCoord)
localRandomCell ao radius = (ao', randCell, randCoord)
    where
        env = aoEnv ao
        originCoord = aoEnvPos ao
        ((randCell, randCoord), ao') = runAgentRandom ao (randomCellWithinRect env originCoord radius)

globalRandomCell :: SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
globalRandomCell ao = (ao', randCell, randCoord)
    where
        env = aoEnv ao
        ((randCell, randCoord), ao') = runAgentRandom ao (randomCell env)
------------------------------------------------------------------------------------------------------------------------

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let aoMoved = if futureOptimizing then segMovementRec ain ao else segMovement ao
        returnA -< aoMoved