module Segregation.Agent (
    isSatisfied,
    isSatisfiedState,
    
    segAgentBehaviour
  ) where

import Segregation.Model

import FRP.Yampa

import FRP.FrABS

import Data.Maybe

------------------------------------------------------------------------------------------------------------------------
-- HELPER-Functions
------------------------------------------------------------------------------------------------------------------------
isOccupied :: SegEnvCell -> Bool
isOccupied = isJust

isSatisfied :: SegAgentOut -> Bool
isSatisfied aout = isSatisfiedState $ agentState aout

isSatisfiedState :: SegAgentState -> Bool
isSatisfiedState s = (segSatisfactionLevel s) >= (segSimilarityWanted s)
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
segMovementRec :: SegEnvironment -> SegAgentIn -> SegAgentOut -> (SegAgentOut, SegEnvironment)
segMovementRec e ain ao 
    -- trace ("Agent " ++ (show $ aoId ao) ++ " isRecursive") 
    | isRecursive ain = (unrecursive ao', e')        -- NOTE: ALWAYS unrecursive, only 1 recursion
    | otherwise = segMovementPresent e ain ao   
    where
        (ao', e') =  segMovementFuture e ain ao

        -- NOTE: we know that the present IS SATISFIED given is environment and that is HAS NOT MOVED
        segMovementFuture :: SegEnvironment -> SegAgentIn -> SegAgentOut -> (SegAgentOut, SegEnvironment)
        segMovementFuture e ain ao
            -- trace ("Agent " ++ (show $ aoId ao) ++ " is satisfied in future, stay in present") 
            | isSatisfied aoFuture = aoe      -- satisfied in the future as well, stay at same position as present
            -- trace ("Agent " ++ (show $ aoId ao) ++ " is not satisfied in future, move in present") 
            | otherwise = move e aoPresent            -- unhappy in the future, move in the present
            where
                -- NOTE: we need the present one with the present environment. although ao has the same state it has NOT the same environment
                aoe@(aoPresent, _) = head $ fromEvent $ agentRecursions ain
                -- NOTE: the aoPresent becomes the future one by calculating the satisfaction-level which depends on the environment which was changed
                -- by the other agents which were simulated before (recursive simulation!)
                aoFuture = updateSatisfactionLevel e ao

        segMovementPresent :: SegEnvironment -> SegAgentIn -> SegAgentOut -> (SegAgentOut, SegEnvironment)
        segMovementPresent e ain ao
            -- trace ("Agent " ++ (show $ aoId ao) ++ " satisfied in present, start recursion") 
            -- NOTE: to reduce the ratio of agents using future-forecasting to 50% instead of all 100% use: && (mod (aoId ao) 2 == 0)
            | isSatisfied ao' 
                && recInitAllowed ain
                = (aor, e')     -- satisfied in the present: check for future, otherwise don't care for future and just move
            | otherwise = (ao', e)
            where
                -- NOTE: this is the decision in the present (non-recursive)
                (ao', e') = segMovement e ao -- trace ("Agent " ++ (show $ aoId ao) ++ " makes move, recAllowed: " ++ (show $ recInitAllowed ain)) 
                aor = recursive False ao'

segMovement :: SegEnvironment -> SegAgentOut -> (SegAgentOut, SegEnvironment)
segMovement e ao 
    | isSatisfied ao' = (ao', e)
    | otherwise = move e ao'
    where
        -- NOTE: need to re-calculate similarity and update agent because could have changed since last update
        ao' = updateSatisfactionLevel e ao

updateSatisfactionLevel :: SegEnvironment -> SegAgentOut -> SegAgentOut
updateSatisfactionLevel e ao = updateAgentState (\s -> s { segSatisfactionLevel = updatedSatisfactionLevel }) ao
    where
        s = agentState ao
        agentPos = segCoord s
        updatedSatisfactionLevel = satisfactionOn e s agentPos

-- NOTE: this function calculates the projected satisfaction of the party if the given coordinate is occupied by this party
satisfactionOn :: SegEnvironment -> SegAgentState -> Discrete2dCoord -> Double
satisfactionOn e s targetCoord
    | Red == party = redFraction
    | otherwise = greenFraction
    where
        party = segParty s
        cs = neighbours targetCoord False e
        (reds, greens) = countOccupied cs

        -- NOTE: if the agent is in the neighbourhood then don't include itself, otherwise include itself (although it might be on the targetcoord, the targetCoord itself is not included)
        includeAgent = if isAgentInNeighbourhood s cs then 0 else 1
        totalCount = (fromInteger $ fromIntegral (reds + greens + includeAgent))
        redFraction = (fromInteger $ fromIntegral (reds + includeAgent)) / totalCount
        greenFraction = (fromInteger $ fromIntegral (greens + includeAgent)) / totalCount

        isAgentInNeighbourhood :: SegAgentState -> [(Discrete2dCoord, SegEnvCell)] -> Bool
        isAgentInNeighbourhood s cs = any ((==(agentPos)) . fst) cs
            where
                agentPos = segCoord s

        countOccupied :: [(Discrete2dCoord, SegEnvCell)] -> (Int, Int)
        countOccupied cs = (redCount, greenCount)
            where
                occupiedCells = filter (isOccupied . snd) cs
                redCount = length $ filter ((==Red) . fromJust . snd) occupiedCells
                greenCount = length $ filter ((==Green) . fromJust . snd) occupiedCells


move :: SegEnvironment -> SegAgentOut -> (SegAgentOut, SegEnvironment)
move e ao = maybe (ao', e) (\optCoord -> moveTo e optCoord ao') mayOptCoord
    where
        (ao', mayOptCoord) = findMove
                                optimizingStrategy
                                movementStrategy
                                selectionStrategy
                                e
                                ao

moveTo :: SegEnvironment -> Discrete2dCoord -> SegAgentOut -> (SegAgentOut, SegEnvironment)
moveTo e newCoord ao = (ao', e'')
    where
        s = agentState ao
        oldCoord = segCoord s
        c = segParty s

        e' = changeCellAt oldCoord Nothing e
        e'' = changeCellAt newCoord (Just c) e'

        ao' = updateAgentState (\s -> s { segCoord = newCoord }) ao

findMove :: SegOptStrategy
                -> SegMoveStrategy
                -> SegSelectionStrategy
                -> SegEnvironment 
                -> SegAgentOut
                -> (SegAgentOut, Maybe Discrete2dCoord)
findMove opts ms SelectNearest e ao = (ao, mayMoveTarget)
    where
        s = agentState ao
        coord = segCoord s
        mayMoveTarget = findNearestFreeCoord ms e (optimizeDistance opts e s) coord

findMove opts ms (SelectRandom optRetries freeCellRetries) e ao = aoRandCoord
    where
        s = agentState ao
        aoRandCoord = findOptRandomFreeCoord ms optRetries freeCellRetries (optimizeSatisfaction opts e s) e ao                                                
        
------------------------------------------------------------------------------------------------------------------------
-- OPTIMIZING
------------------------------------------------------------------------------------------------------------------------
moveAlways :: SegEnvironment -> SegAgentState -> Discrete2dCoord -> Bool
moveAlways _ _ _ = True

moveSatisfies :: SegEnvironment -> SegAgentState -> Discrete2dCoord -> Bool
moveSatisfies e s targetCoord = similiarityOnCoord > similarityWanted
    where
        similiarityOnCoord = satisfactionOn e s targetCoord
        similarityWanted = segSimilarityWanted s

moveImproves :: SegEnvironment -> SegAgentState -> Discrete2dCoord -> Bool
moveImproves e s targetCoord = satisfactionLevelOnCoord > satisfactionLevel
    where
        satisfactionLevelOnCoord = satisfactionOn e s targetCoord
        satisfactionLevel = segSatisfactionLevel s

optimizeSatisfaction :: SegOptStrategy -> SegEnvironment -> SegAgentState -> Discrete2dCoord -> Bool
optimizeSatisfaction OptNone = moveAlways
optimizeSatisfaction OptSimilaritySatisfied = moveSatisfies
optimizeSatisfaction OptSimilarityIncreasing = moveImproves

optimizeDistance :: SegOptStrategy
                        -> SegEnvironment 
                        -> SegAgentState
                        -> (Discrete2dCoord, SegEnvCell) 
                        -> (Discrete2dCoord, SegEnvCell) 
                        -> (Discrete2dCoord, SegEnvCell)
optimizeDistance opts e s best@(bestCoord, _) comp@(compCoord, _)
    | distComp < distBest = if improves then comp else best
    | otherwise = best
    where
        agentPos = segCoord s
        improves = optimizeSatisfaction opts e s compCoord
        distBest = distanceManhattanDisc2d agentPos bestCoord
        distComp = distanceManhattanDisc2d agentPos compCoord
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Nearest Cell Selection
------------------------------------------------------------------------------------------------------------------------
findNearestFreeCoord :: SegMoveStrategy
                        -> SegEnvironment
                        -> ((Discrete2dCoord, SegEnvCell) -> (Discrete2dCoord, SegEnvCell) -> (Discrete2dCoord, SegEnvCell))
                        -> Discrete2dCoord
                        -> Maybe Discrete2dCoord
findNearestFreeCoord strat e optFunc coord = maybe Nothing (Just . fst) mayNearest
    where
        coordsCellsSearch = case strat of   (MoveLocal r) -> cellsAroundRect coord r e
                                            MoveGlobal -> allCellsWithCoords e

        mayNearest = foldr (findNearestAux optFunc) Nothing coordsCellsSearch

findNearestAux :: ((Discrete2dCoord, SegEnvCell) -> (Discrete2dCoord, SegEnvCell) -> (Discrete2dCoord, SegEnvCell))
                    -> (Discrete2dCoord, SegEnvCell)
                    -> Maybe (Discrete2dCoord, SegEnvCell)
                    -> Maybe (Discrete2dCoord, SegEnvCell)
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
findOptRandomFreeCoord :: SegMoveStrategy
                            -> Int
                            -> Int
                            -> (Discrete2dCoord -> Bool)
                            -> SegEnvironment
                            -> SegAgentOut
                            -> (SegAgentOut, Maybe Discrete2dCoord)
findOptRandomFreeCoord ms 0 freeCellRetries _ e ao = findRandomFreeCoord ms freeCellRetries e ao
findOptRandomFreeCoord ms optRetries freeCellRetries optFunc e ao =
        maybe ret (\freeCoord -> if optFunc freeCoord then
                                    ret
                                    else
                                        findOptRandomFreeCoord ms (optRetries - 1) freeCellRetries optFunc e ao') mayFreeCoord
    where
        ret@(ao', mayFreeCoord) = findRandomFreeCoord ms freeCellRetries e ao

findRandomFreeCoord :: SegMoveStrategy -> Int -> SegEnvironment -> SegAgentOut -> (SegAgentOut, Maybe Discrete2dCoord)
findRandomFreeCoord _ 0 e ao = (ao, Nothing)
findRandomFreeCoord ms maxRetries e ao
    | isOccupied randCell = findRandomFreeCoord ms (maxRetries - 1) e ao'
    | otherwise = (ao', Just randCoord)
    where
        (ao', randCell, randCoord) = findRandomFreeCoordAux ms e ao

        findRandomFreeCoordAux :: SegMoveStrategy -> SegEnvironment -> SegAgentOut -> (SegAgentOut, SegEnvCell, Discrete2dCoord)
        findRandomFreeCoordAux (MoveLocal radius) e ao = localRandomCell e radius ao
        findRandomFreeCoordAux MoveGlobal e ao = globalRandomCell e ao

localRandomCell :: SegEnvironment -> Int -> SegAgentOut -> (SegAgentOut, SegEnvCell, Discrete2dCoord)
localRandomCell e radius ao = (ao', randCell, randCoord)
    where
        originCoord = segCoord $ agentState ao
        ((randCell, randCoord), ao') = agentRandom (randomCellWithinRect originCoord radius e) ao

globalRandomCell :: SegEnvironment -> SegAgentOut -> (SegAgentOut, SegEnvCell, Discrete2dCoord)
globalRandomCell e ao = (ao', randCell, randCoord)
    where
        ((randCell, randCoord), ao') = agentRandom (randomCell e) ao
------------------------------------------------------------------------------------------------------------------------

segAgentBehaviourFunc :: SegAgentPureBehaviour
segAgentBehaviourFunc e _ ain ao 
    | futureOptimizing = segMovementRec e ain ao
    | otherwise = segMovement e ao

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = agentPure segAgentBehaviourFunc