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
import Control.Monad.Random
import Control.Monad

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentDies :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDies = unoccupyPosition . kill

passWealthOn :: SugarScapeAgentOut -> SugarScapeAgentOut
passWealthOn a
    | null childrenIds = a
    | otherwise = broadcastMessage a (InheritSugar childrenSugarShare) childrenIds
    where
        s = aoState a
        sugarLevel = sugAgSugarLevel s

        childrenIds = sugAgChildren s
        childrenCount = length childrenIds
        childrenSugarShare = sugarLevel / (fromRational $ toRational $ fromIntegral childrenCount)

unoccupyPosition ::  SugarScapeAgentOut -> SugarScapeAgentOut
unoccupyPosition a = a { aoEnv = env' }
    where
        (cellCoord, cell) = agentCell a
        cellUnoccupied = cell { sugEnvOccupier = Nothing }
        env' = changeCellAt (aoEnv a) cellCoord cellUnoccupied

starvedToDeath :: SugarScapeAgentOut -> Bool
starvedToDeath a = (sugAgSugarLevel $ aoState a) <= 0

agentMetabolism :: SugarScapeAgentOut -> SugarScapeAgentOut
agentMetabolism a 
    | starvedToDeath a1 = agentDies a1
    | otherwise = a1
    where
        metab = sugAgMetabolism $ aoState a

        a0 = updateState a (\s -> s {
                                sugAgSugarLevel =
                                    max
                                        0
                                        ((sugAgSugarLevel s) - metab)})

        pol = metab * polutionMetabolismFactor 
        cell = agentCell a0
        a1 = agentPoluteCell pol cell a0

agentNonCombatMove :: SugarScapeAgentOut -> SugarScapeAgentOut
agentNonCombatMove a
    | null unoccupiedCells = agentStayAndHarvest a
    | otherwise = agentMoveAndHarvestCell a' cellCoord
    where
        cellsInSight = agentLookout a
        unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

        bestCells = selectBestCells (aoEnvPos a) unoccupiedCells
        ((cellCoord, _), a') = agentPickRandom a bestCells

agentMoveAndHarvestCell :: SugarScapeAgentOut -> EnvCoord -> SugarScapeAgentOut
agentMoveAndHarvestCell a cellCoord = a1
    where
        a0 = agentHarvestCell a cellCoord
        a1 = agentMoveTo a0 cellCoord

agentStayAndHarvest :: SugarScapeAgentOut -> SugarScapeAgentOut
agentStayAndHarvest a = agentHarvestCell a cellCoord
    where
        (cellCoord, _) = agentCell a

agentCell :: SugarScapeAgentOut -> (EnvCoord, SugarScapeEnvCell)
agentCell a = (agentPos, cellOfAgent)
    where
        env = aoEnv a
        agentPos = aoEnvPos a
        cellOfAgent = cellAt env agentPos

agentPoluteCell :: Double -> (EnvCoord, SugarScapeEnvCell) -> SugarScapeAgentOut -> SugarScapeAgentOut
agentPoluteCell polutionIncrease (cellCoord, cell) a 
    | polutionEnabled = a { aoEnv = env }
    | otherwise = a
    where
        cellAfterPolution = cell {
            sugEnvPolutionLevel = polutionIncrease + (sugEnvPolutionLevel cell)
        }
        env = changeCellAt (aoEnv a) cellCoord cellAfterPolution

agentHarvestCell  :: SugarScapeAgentOut -> EnvCoord -> SugarScapeAgentOut
agentHarvestCell a cellCoord = a2
    where
        cell = cellAt (aoEnv a) cellCoord

        sugarLevelCell = sugEnvSugarLevel cell
        sugarLevelAgent = sugAgSugarLevel $ aoState a
        newSugarLevelAgent = (sugarLevelCell + sugarLevelAgent)

        a0 = updateState a (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

        cellHarvested = cell { sugEnvSugarLevel = 0.0 }
        env = changeCellAt (aoEnv a0) cellCoord cellHarvested
        a1 = a0 { aoEnv = env }

        pol = sugarLevelCell * polutionHarvestFactor 
        a2 = agentPoluteCell pol (cellCoord, cellHarvested) a1

agentMoveTo :: SugarScapeAgentOut -> EnvCoord -> SugarScapeAgentOut
agentMoveTo a cellCoord = a0 { aoEnvPos = cellCoord, aoEnv = env }
    where
        a0 = unoccupyPosition a
        cell = cellAt (aoEnv a0) cellCoord
        cellOccupied = cell { sugEnvOccupier = Just (cellOccupier (aoId a0) (aoState a0))}
        env = changeCellAt (aoEnv a0) cellCoord cellOccupied

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

agentAgeing :: Double -> SugarScapeAgentOut -> SugarScapeAgentOut
agentAgeing newAge a
    | dieFromAge a = agentDies $ passWealthOn a' -- $ birthNewAgent a
    | otherwise = a'
    where
        a' = updateState a (\s -> s { sugAgAge = newAge })

birthNewAgent :: SugarScapeAgentOut -> SugarScapeAgentOut
birthNewAgent a = createAgent a1 newAgentDef
    where
        newAgentId = aoId a                                 -- NOTE: we keep the old id
        (newAgentCoord, a0) = findUnoccpiedRandomPosition a
        (newAgentDef, a1) = runAgentRandom a0
            (randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviour sugarScapeAgentConversation)

        findUnoccpiedRandomPosition :: SugarScapeAgentOut -> (EnvCoord, SugarScapeAgentOut)
        findUnoccpiedRandomPosition a
            | cellOccupied c = findUnoccpiedRandomPosition a'
            | otherwise = (coord, a')
            where
                ((c, coord), a') = runAgentRandom a (randomCell (aoEnv a))
                
dieFromAge :: SugarScapeAgentOut -> Bool
dieFromAge a = age > maxAge
    where
        s = aoState a
        age = sugAgAge s
        maxAge = sugAgMaxAge s
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSex :: SugarScapeAgentOut -> SugarScapeAgentOut
agentSex a
    | isFertile s = agentMatingConversation nids nncsUnoccupied a
    | otherwise = a
    where
        s = aoState a
        pos = aoEnvPos a
        env = aoEnv a

        neighbourCells = neighbours env pos
        nids = neighbourIds a

        -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
        nncsDupl = foldr (\(coord, _) acc -> (neighbours env coord) ++ acc) neighbourCells neighbourCells
        -- NOTE: the nncs are not unique, remove duplicates
        nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl
        nncsUnoccupied = filter (isNothing . sugEnvOccupier . snd) nncsUnique

        agentMatingConversation :: [AgentId]
                                        -> [(EnvCoord, SugarScapeEnvCell)]
                                        -> SugarScapeAgentOut
                                        -> SugarScapeAgentOut
        agentMatingConversation [] _ a = conversationEnd a
        agentMatingConversation _ [] a = conversationEnd a
        agentMatingConversation (receiverId:otherAis) allCoords@((coord, cell):cs) a
            | satisfiesWealthForChildBearing s = conversation a (receiverId, m) agentMatingConversationsReply
            | otherwise = conversationEnd a
            where
                s = aoState a
                m =  MatingRequest (sugAgGender $ s) --trace ("MatingRequest to " ++ (show receiverId)) MatingRequest (sugAgGender $ s)

                agentMatingConversationsReply :: SugarScapeAgentOut
                                                    -> Maybe (AgentMessage SugarScapeMsg)
                                                    -> SugarScapeAgentOut
                agentMatingConversationsReply a Nothing = agentMatingConversation otherAis allCoords a  -- NOTE: the target was not found or does not have a handler, continue with the next
                agentMatingConversationsReply a (Just (_, MatingReplyNo)) = agentMatingConversation otherAis allCoords a
                agentMatingConversationsReply a (Just (senderId, (MatingReplyYes otherTup))) = 
                    conversation a3 (receiverId, (MatingChild newBornId)) (\a' _ -> agentMatingConversation otherAis cs a')
                    
                    where
                        s = aoState a
                        initialSugarEndow = sugAgSugarInit s
                        sugarLevel = sugAgSugarLevel s

                        mySugarContribution = initialSugarEndow / 2.0
                        myMetab = sugAgMetabolism s
                        myVision = sugAgVision s
                        myCulturalTag = sugAgCulturalTag s

                        newBornId = senderId * aoId a   -- TODO: this is a real problem: which ids do we give our newborns?

                        (newBornDef, a0) = runAgentRandom a
                            (createNewBorn 
                                (newBornId, coord)
                                (mySugarContribution, myMetab, myVision, myCulturalTag)
                                otherTup)

                        env = aoEnv a0
                        cell' = cell { sugEnvOccupier = Just (cellOccupier newBornId (adState newBornDef))}
                        env' = changeCellAt env coord cell'

                        a1 = a0 { aoEnv = env' }
                        a2 = updateState a1 (\s -> s { sugAgSugarLevel = sugarLevel - mySugarContribution,
                                                        sugAgChildren = newBornId : (sugAgChildren s)})
                        a3 = createAgent a2 newBornDef

                agentMatingConversationsReply a (Just (_, _)) = agentMatingConversation otherAis allCoords a  -- NOTE: unexpected/MatingChildAck reply, continue with the next

createNewBorn :: (AgentId, EnvCoord)
                    -> (Double, Double, Int, SugarScapeCulturalTag)
                    -> (Double, Double, Int, SugarScapeCulturalTag)
                    -> Rand StdGen SugarScapeAgentDef
createNewBorn idCoord
                (sugEndowFather, metabFather, visionFather, cultureFather)
                (sugEndowMother, metabMother, visionMother, cultureMother) =
    do
        newBornMetabolism <- crossover (metabFather, metabMother)
        newBornVision <- crossover (visionFather, visionMother)
        newBornCulturalTag <- culturalCrossover cultureFather cultureMother

        let newBornSugarEndow = sugEndowFather + sugEndowMother

        newBornDef <- randomAgent
                            idCoord
                            sugarScapeAgentBehaviour
                            sugarScapeAgentConversation

        let newBornState' = (adState newBornDef) { sugAgMetabolism = newBornMetabolism,
                                                   sugAgVision = newBornVision,
                                                   sugAgSugarInit = newBornSugarEndow,
                                                   sugAgCulturalTag = newBornCulturalTag,
                                                   sugAgTribe = calculateTribe newBornCulturalTag }

        return newBornDef { adState = newBornState' }

satisfiesWealthForChildBearing :: SugarScapeAgentState -> Bool
satisfiesWealthForChildBearing s = currSugar >= initSugar
    where
        currSugar = sugAgSugarLevel s
        initSugar = sugAgSugarInit s

isFertile :: SugarScapeAgentState -> Bool
isFertile s = withinRange age fertilityAgeRange
    where
        age = sugAgAge s
        fertilityAgeRange = sugAgFertAgeRange s

withinRange :: (Ord a) => a -> (a, a) -> Bool
withinRange a (l, u) = a >= l && a <= u

inheritSugar :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
inheritSugar ain a = onMessage inheritSugarMatch ain inheritSugarAction a
    where
        inheritSugarAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        inheritSugarAction a (_, (InheritSugar sug)) = updateState a (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + sug})

        inheritSugarMatch :: AgentMessage SugarScapeMsg -> Bool
        inheritSugarMatch (_, InheritSugar _) = True
        inheritSugarMatch _ = False

agentCultureContact :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCultureContact ain a = broadcastMessage a' (CulturalContact culturalTag) nids 
    where
        a' = onMessage cultureContactMatch ain cultureContactAction a
        nids = neighbourIds a'
        culturalTag = sugAgCulturalTag $ aoState a'

        cultureContactMatch :: AgentMessage SugarScapeMsg -> Bool
        cultureContactMatch (_, CulturalContact _) = True
        cultureContactMatch _ = False

        cultureContactAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        cultureContactAction a (_, (CulturalContact tagActive)) = 
                updateState a' (\s -> s { sugAgCulturalTag = agentTag',
                                            sugAgTribe = tribe})
            where
                s = aoState a
                agentTag = sugAgCulturalTag s
                (agentTag', a') = runAgentRandom a (cultureContact tagActive agentTag)
                tribe = calculateTribe agentTag'

agentKilledInCombat :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentKilledInCombat ain a = onMessage killedInCombatMatch ain killedInCombatAction a
    where
        killedInCombatAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        killedInCombatAction a (_, KilledInCombat) = kill a -- NOTE: don't unoccupie position (as in agentdies) because it is occupied by the killer already

        killedInCombatMatch :: AgentMessage SugarScapeMsg -> Bool
        killedInCombatMatch (_, KilledInCombat) = True
        killedInCombatMatch _ = False

agentCombatMove :: SugarScapeAgentOut -> SugarScapeAgentOut
agentCombatMove a 
    | null targetCells = agentStayAndHarvest a
    | otherwise = if vulnerableToRetaliation payoff a' then agentStayAndHarvest a' else moveAndHarvestBestCell bestCell a'
    where
        s = aoState a
        agentPos = aoEnvPos a
        myTribe = sugAgTribe s
        myWealth = sugAgSugarLevel s 

        cellsInSight = agentLookout a
        targetCells = filter (filterTargetCell occupierCombatable) cellsInSight
        targeCellsWithPayoff = map cellPayoff targetCells

        cellsSortedByPayoff = sortBy (\c1 c2 -> compare (snd c2) (snd c1)) targeCellsWithPayoff
        bestCellPayoff = snd $ head cellsSortedByPayoff
        bestCells = filter ((==bestCellPayoff) . snd) cellsSortedByPayoff

        shortestDistanceBestCells = sortBy (\c1 c2 -> compare (distance agentPos (fst . fst $ c1)) (distance agentPos (fst . fst $ c2))) bestCells
        shortestDistance = distance agentPos (fst . fst $ head shortestDistanceBestCells)
        bestShortestDistanceCells = filter ((==shortestDistance) . (distance agentPos) . fst . fst) shortestDistanceBestCells

        (bestCell@((_,_), payoff), a') = agentPickRandom a bestShortestDistanceCells
        
        -- NOTE: calculate if retalion is possible: is there an agent of the other tribe in my vision which is wealthier AFTER i have preyed on the current one?
        -- TODO: this is not very well specified in the SugarScape book. we don't know the vision of the other agent, and its information we should not have access to
        vulnerableToRetaliation :: Double -> SugarScapeAgentOut -> Bool
        vulnerableToRetaliation payoff a = (not . null) retaliatingCells
            where
                sugarLevelAgent = sugAgSugarLevel $ aoState a
                futureSugarLevel = (payoff + sugarLevelAgent)

                cellsInSight = agentLookout a
                retaliatingCells = filter (filterTargetCell (occupierRetaliator futureSugarLevel)) cellsInSight

        moveAndHarvestBestCell :: ((EnvCoord, SugarScapeEnvCell), Double) -> SugarScapeAgentOut -> SugarScapeAgentOut
        moveAndHarvestBestCell ((cellCoord, cell), payoff) a 
            | cellOccupied cell = killOccupierOfCell a'' cell
            | otherwise = a''
                where
                    sugarLevelAgent = sugAgSugarLevel $ aoState a
                    newSugarLevelAgent = (payoff + sugarLevelAgent)

                    a' = unoccupyPosition $ updateState a (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

                    cellHarvestedAndOccupied = cell {
                            sugEnvSugarLevel = 0.0,
                            sugEnvOccupier = Just (cellOccupier (aoId a') (aoState a')),
                            sugEnvPolutionLevel = 0
                    }
                            
                    env = changeCellAt (aoEnv a') cellCoord cellHarvestedAndOccupied
                    a'' = a' { aoEnvPos = cellCoord, aoEnv = env }

        killOccupierOfCell :: SugarScapeAgentOut -> SugarScapeEnvCell -> SugarScapeAgentOut
        killOccupierOfCell a cell = sendMessage a (occupierId, KilledInCombat)
            where
                occupier = fromJust $ sugEnvOccupier cell
                occupierId = sugEnvOccId occupier 

        filterTargetCell :: (SugarScapeEnvCellOccupier -> Bool) -> (EnvCoord, SugarScapeEnvCell) -> Bool
        filterTargetCell f (coord, cell) = maybe True f mayOccupier
            where
                mayOccupier = sugEnvOccupier cell

        occupierCombatable :: SugarScapeEnvCellOccupier -> Bool
        occupierCombatable occupier = differentTribe && lessWealthy
            where
                otherTribe = sugEnvOccTribe occupier
                otherWealth = sugEnvOccWealth occupier
                differentTribe = otherTribe /= myTribe
                lessWealthy = otherWealth <myWealth 

        occupierRetaliator :: Double -> SugarScapeEnvCellOccupier -> Bool
        occupierRetaliator referenceWealth occupier = differentTribe && moreWealthy
            where
                otherTribe = sugEnvOccTribe occupier
                otherWealth = sugEnvOccWealth occupier
                differentTribe = otherTribe /= myTribe
                moreWealthy = otherWealth > referenceWealth 

        cellPayoff :: (EnvCoord, SugarScapeEnvCell) -> ((EnvCoord, SugarScapeEnvCell), Double)
        cellPayoff (c, cell) = ((c, cell), payoff)
            where
                mayOccupier = sugEnvOccupier cell
                sugarLevel = sugEnvSugarLevel cell
                payoff = maybe sugarLevel (\occupier -> sugarLevel + (min combatReward (sugEnvOccWealth occupier))) mayOccupier
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- GENERAL AGENT-RELATED
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentConversation :: SugarScapeAgentConversation
sugarScapeAgentConversation ain (_, (MatingRequest tup)) = Just (m, ain')
    where 
        (m, ain') = handleMatingConversation tup ain
sugarScapeAgentConversation ain (_, (MatingChild childId)) = Just (MatingChildAck, ain')
    where
        s = aiState ain
        s' = s { sugAgChildren = childId : (sugAgChildren s)}
        ain' = ain { aiState = s' }
sugarScapeAgentConversation ain _ = Nothing

handleMatingConversation :: (SugarScapeAgentGender)
                                -> SugarScapeAgentIn
                                -> (SugarScapeMsg, SugarScapeAgentIn)
handleMatingConversation otherGender ain 
    | isFertile s &&
        satisfiesWealthForChildBearing s &&
        differentGender = (MatingReplyYes (mySugarContribution, myMetab, myVision, myCulturalTag), ain')
    | otherwise = (MatingReplyNo, ain)
    where
        s = aiState ain
        myGender = sugAgGender s
        differentGender = myGender /= otherGender

        -- NOTE: to be fertile an agent must have at least as much sugar as initially endowed, therefore it cannot go negative
        initialSugarEndow = sugAgSugarInit s
        sugarLevel = sugAgSugarLevel s
        mySugarContribution = initialSugarEndow / 2.0
        myMetab = sugAgMetabolism s
        myVision = sugAgVision s
        myCulturalTag = sugAgCulturalTag s
        
        s' = s { sugAgSugarLevel = sugarLevel - mySugarContribution }
        ain' = ain { aiState = s'}

neighbourIds :: SugarScapeAgentOut -> [AgentId]
neighbourIds a = map (sugEnvOccId . fromJust . sugEnvOccupier . snd) occupiedCells
    where
        env = aoEnv a
        pos = aoEnvPos a
        neighbourCells = neighbours env pos
        occupiedCells = filter (isJust . sugEnvOccupier . snd) neighbourCells

sugarScapeAgentBehaviourFunc :: Double -> SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut 
sugarScapeAgentBehaviourFunc age ain a = do     
                                            let a0 = agentKilledInCombat ain a 
                                            if isDead a0 then
                                                a0
                                                else
                                                    do
                                                        let a1 = agentAgeing age a0
                                                        if isDead a1 then
                                                            a1
                                                            else
                                                                do
                                                                    let a2 = agentMetabolism a1
                                                                    if isDead a2 then
                                                                        a2
                                                                        else 
                                                                            do
                                                                                let a3 = agentNonCombatMove a2
                                                                                let a4 = inheritSugar ain a3
                                                                                let a5 = agentCultureContact ain a4
                                                                                let a6 = agentSex a5
                                                                                a6


sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = proc ain ->
    do
        age <- time -< 0

        let a = agentOutFromIn ain
        returnA -< sugarScapeAgentBehaviourFunc age ain a

------------------------------------------------------------------------------------------------------------------------