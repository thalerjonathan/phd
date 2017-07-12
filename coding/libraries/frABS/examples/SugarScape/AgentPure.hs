module SugarScape.AgentPure (
    sugarScapeAgentConversationPure,
    sugarScapeAgentBehaviourPure
  ) where

import SugarScape.Model
import SugarScape.Environment
import SugarScape.AgentCommon

import FRP.FrABS

import Data.Maybe
import Data.List

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR NON-MONADIC 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentDies :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDies = unoccupyPosition . kill

passWealthOn :: SugarScapeAgentOut -> SugarScapeAgentOut
passWealthOn a
    | null childrenIds = a
    | otherwise = broadcastMessage (InheritSugar childrenSugarShare) childrenIds a 
    where
        s = aoState a
        sugarLevel = sugAgSugarLevel s

        childrenIds = sugAgChildren s
        childrenCount = length childrenIds
        childrenSugarShare = sugarLevel / (fromRational $ toRational $ fromIntegral childrenCount)

unoccupyPosition ::  SugarScapeAgentOut -> SugarScapeAgentOut
unoccupyPosition a = a { aoEnv = env' }
    where
        (cellCoord, cell) = agentCellOnPos a
        cellUnoccupied = cell { sugEnvOccupier = Nothing }
        env' = changeCellAt cellCoord cellUnoccupied (aoEnv a)

starvedToDeath :: SugarScapeAgentOut -> Bool
starvedToDeath a = noSugar || noSpice
    where
        noSugar = (sugAgSugarLevel $ aoState a) <= 0
        noSpice = (sugAgSpiceLevel $ aoState a) <= 0

agentMetabolism :: SugarScapeAgentOut -> SugarScapeAgentOut
agentMetabolism a 
    | starvedToDeath a1 = agentDies a1
    | otherwise = a1
    where
        s = aoState a
        (sugarMetab, spiceMetab) = metabolismAmount s

        newSugarLevel = max 0 ((sugAgSugarLevel s) - sugarMetab)
        newSpiceLevel = max 0 ((sugAgSpiceLevel s) - spiceMetab)

        a0 = updateDomainState (\s -> s { sugAgSugarLevel = newSugarLevel,
                                        sugAgSpiceLevel = newSpiceLevel }) a 

        -- NOTE: for now the metabolism (and harvest) of spice does not cause any polution
        pol = sugarMetab * polutionMetabolismFactor
        cell = agentCellOnPos a0
        a1 = agentPoluteCell pol cell a0

agentNonCombatMove :: SugarScapeAgentOut -> SugarScapeAgentOut
agentNonCombatMove a
    | null unoccupiedCells = agentStayAndHarvest a
    | otherwise = agentMoveAndHarvestCell a' cellCoord
    where
        cellsInSight = agentLookout a
        unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

        refCoord = aoEnvPos a
        bestCells = selectBestCells bestMeasureSugarLevel refCoord unoccupiedCells
        ((cellCoord, _), a') = agentPickRandom bestCells a 

agentMoveAndHarvestCell :: SugarScapeAgentOut 
                            -> EnvCoord 
                            -> SugarScapeAgentOut
agentMoveAndHarvestCell a cellCoord = a1
    where
        a0 = agentHarvestCell a cellCoord
        a1 = agentMoveTo a0 cellCoord

agentStayAndHarvest :: SugarScapeAgentOut -> SugarScapeAgentOut
agentStayAndHarvest a = agentHarvestCell a cellCoord
    where
        (cellCoord, _) = agentCellOnPos a

agentPoluteCell :: Double 
                    -> (EnvCoord, SugarScapeEnvCell) 
                    -> SugarScapeAgentOut 
                    -> SugarScapeAgentOut
agentPoluteCell polutionIncrease (cellCoord, cell) a 
    | polutionEnabled = a { aoEnv = env }
    | otherwise = a
    where
        cellAfterPolution = cell {
            sugEnvPolutionLevel = polutionIncrease + (sugEnvPolutionLevel cell)
        }
        env = changeCellAt cellCoord cellAfterPolution (aoEnv a) 

agentHarvestCell  :: SugarScapeAgentOut 
                        -> EnvCoord 
                        -> SugarScapeAgentOut
agentHarvestCell a cellCoord = a2
    where
        cell = cellAt cellCoord (aoEnv a) 

        sugarLevelCell = sugEnvSugarLevel cell
        sugarLevelAgent = sugAgSugarLevel $ aoState a
        newSugarLevelAgent = sugarLevelCell + sugarLevelAgent

        spiceLevelCell = sugEnvSpiceLevel cell
        spiceLevelAgent = sugAgSpiceLevel $ aoState a
        newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

        a0 = updateDomainState (\s -> s { sugAgSugarLevel = newSugarLevelAgent,
                                        sugAgSpiceLevel = newSpiceLevelAgent }) a 

        cellHarvested = cell { sugEnvSugarLevel = 0.0,
                                 sugEnvSpiceLevel = 0.0 }
        env = changeCellAt cellCoord cellHarvested (aoEnv a0) 
        a1 = a0 { aoEnv = env }

        -- NOTE: at the moment harvesting SPICE does not influence the polution
        pol = sugarLevelCell * polutionHarvestFactor 
        a2 = agentPoluteCell pol (cellCoord, cellHarvested) a1

agentMoveTo :: SugarScapeAgentOut -> EnvCoord -> SugarScapeAgentOut
agentMoveTo a cellCoord = a0 { aoEnvPos = cellCoord, aoEnv = env }
    where
        a0 = unoccupyPosition a
        cell = cellAt cellCoord (aoEnv a0)
        cellOccupied = cell { sugEnvOccupier = Just (cellOccupier (aoId a0) (aoState a0))}
        env = changeCellAt cellCoord cellOccupied (aoEnv a0) 


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
        visionCells = cellsAt visionCoordsWrapped env

agentAgeing :: Double 
                -> SugarScapeAgentOut 
                -> SugarScapeAgentOut
agentAgeing newAge a
    | dieFromAge a = agentDies $ passWealthOn a' -- $ birthNewAgent a
    | otherwise = a'
    where
        a' = updateDomainState (\s -> s { sugAgAge = newAge }) a 

birthNewAgent :: SugarScapeAgentOut -> SugarScapeAgentOut
birthNewAgent a = createAgent newAgentDef a1 
    where
        newAgentId = aoId a                                 -- NOTE: we keep the old id
        (newAgentCoord, a0) = findUnoccpiedRandomPosition a
        (newAgentDef, a1) = runAgentRandom 
            (randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviourPure sugarScapeAgentConversationPure)
            a0

        findUnoccpiedRandomPosition :: SugarScapeAgentOut -> (EnvCoord, SugarScapeAgentOut)
        findUnoccpiedRandomPosition a
            | cellOccupied c = findUnoccpiedRandomPosition a'
            | otherwise = (coord, a')
            where
                ((c, coord), a') = runAgentRandom (randomCell (aoEnv a)) a 
                
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
agentSex :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentSex ain a
    | isFertile s = agentMatingConversation nids nncsUnoccupied a
    | otherwise = a
    where
        s = aoState a
        pos = aoEnvPos a
        env = aoEnv a

        neighbourCells = neighbours pos env
        nids = neighbourIds a

        -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
        nncsDupl = foldr (\(coord, _) acc -> (neighbours coord env) ++ acc) neighbourCells neighbourCells
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
            | satisfiesWealthForChildBearing s = conversation (receiverId, m) agentMatingConversationsReply a 
            | otherwise = conversationEnd a
            where
                s = aoState a
                m =  MatingRequest (sugAgGender $ s) --trace ("MatingRequest to " ++ (show receiverId)) MatingRequest (sugAgGender $ s)

                agentMatingConversationsReply :: SugarScapeAgentConversationSender
                agentMatingConversationsReply a Nothing = agentMatingConversation otherAis allCoords a  -- NOTE: the target was not found or does not have a handler, continue with the next
                agentMatingConversationsReply a (Just (_, MatingReplyNo)) = agentMatingConversation otherAis allCoords a
                agentMatingConversationsReply a (Just (senderId, (MatingReplyYes otherTup))) = 
                    conversation (receiverId, (MatingChild newBornId)) (\a' _ -> agentMatingConversation otherAis cs a') a3
                    
                    where
                        s = aoState a
                        initialSugarEndow = sugAgSugarInit s
                        sugarLevel = sugAgSugarLevel s

                        mySugarContribution = initialSugarEndow / 2.0
                        mySugarMetab = sugAgSugarMetab s
                        mySpiceMetab = sugAgSpiceMetab s
                        myVision = sugAgVision s
                        myCulturalTag = sugAgCulturalTag s
                        myImmuneSysBorn = sugAgImmuneSysBorn s

                        newBornId = nextAgentId ain -- senderId * aoId a   -- TODO: this is a real problem: which ids do we give our newborns?

                        (newBornDef, a0) = runAgentRandom 
                            (createNewBorn 
                                (newBornId, coord)
                                (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn)
                                otherTup
                                sugarScapeAgentBehaviourPure
                                sugarScapeAgentConversationPure)
                            a

                        env = aoEnv a0
                        cell' = cell { sugEnvOccupier = Just (cellOccupier newBornId (adState newBornDef))}
                        env' = changeCellAt  coord cell' env

                        a1 = a0 { aoEnv = env' }
                        a2 = updateDomainState (\s -> s { sugAgSugarLevel = sugarLevel - mySugarContribution,
                                                        sugAgChildren = newBornId : (sugAgChildren s)}) a1
                        a3 = createAgent newBornDef a2

                agentMatingConversationsReply a (Just (_, _)) = agentMatingConversation otherAis allCoords a  -- NOTE: unexpected/MatingChildAck reply, continue with the next


inheritSugar :: SugarScapeAgentIn 
                -> SugarScapeAgentOut 
                -> SugarScapeAgentOut
inheritSugar ain a = onMessage inheritSugarAction ain a
    where
        inheritSugarAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        inheritSugarAction a (_, (InheritSugar sug)) = updateDomainState (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + sug}) a 
        inheritSugarAction a _ = a

handleMatingConversation :: (SugarScapeAgentGender)
                                -> SugarScapeAgentIn
                                -> (SugarScapeMsg, SugarScapeAgentIn)
handleMatingConversation otherGender ain 
    | isFertile s &&
        satisfiesWealthForChildBearing s &&
        differentGender = (MatingReplyYes (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn), ain')
    | otherwise = (MatingReplyNo, ain)
    where
        s = aiState ain
        myGender = sugAgGender s
        differentGender = myGender /= otherGender

        -- NOTE: to be fertile an agent must have at least as much sugar as initially endowed, therefore it cannot go negative
        initialSugarEndow = sugAgSugarInit s
        sugarLevel = sugAgSugarLevel s
        mySugarContribution = initialSugarEndow / 2.0
        mySugarMetab = sugAgSugarMetab s
        mySpiceMetab = sugAgSpiceMetab s
        myVision = sugAgVision s
        myCulturalTag = sugAgCulturalTag s
        myImmuneSysBorn = sugAgImmuneSysBorn s

        s' = s { sugAgSugarLevel = sugarLevel - mySugarContribution }
        ain' = ain { aiState = s'}

agentCultureContact :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCultureContact ain a = broadcastMessage (CulturalContact culturalTag) nids a' 
    where
        a' = onMessage cultureContactAction ain a
        nids = neighbourIds a'
        culturalTag = sugAgCulturalTag $ aoState a'

        cultureContactAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        cultureContactAction a (_, (CulturalContact tagActive)) = 
                updateDomainState (\s -> s { sugAgCulturalTag = agentTag',
                                            sugAgTribe = tribe}) a' 
            where
                s = aoState a
                agentTag = sugAgCulturalTag s
                (agentTag', a') = runAgentRandom (cultureContact tagActive agentTag) a 
                tribe = calculateTribe agentTag'
        cultureContactAction a _ = a

agentKilledInCombat :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentKilledInCombat ain a = onMessage killedInCombatAction ain a
    where
        killedInCombatAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        killedInCombatAction a (_, KilledInCombat) = kill a -- NOTE: don't unoccupie position (as in agentdies) because it is occupied by the killer already
        killedInCombatAction a _ = a

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
        targetCells = filter (filterTargetCell (occupierCombatable myWealth myTribe)) cellsInSight
        targeCellsWithPayoff = map cellPayoff targetCells

        cellsSortedByPayoff = sortBy (\c1 c2 -> compare (snd c2) (snd c1)) targeCellsWithPayoff
        bestCellPayoff = snd $ head cellsSortedByPayoff
        bestCells = filter ((==bestCellPayoff) . snd) cellsSortedByPayoff

        shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattan agentPos (fst . fst $ c1)) (distanceManhattan agentPos (fst . fst $ c2))) bestCells
        shortestdistanceManhattan = distanceManhattan agentPos (fst . fst $ head shortestdistanceManhattanBestCells)
        bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattan agentPos) . fst . fst) shortestdistanceManhattanBestCells

        (bestCell@((_,_), payoff), a') = agentPickRandom bestShortestdistanceManhattanCells a 
        
        -- NOTE: calculate if retalion is possible: is there an agent of the other tribe in my vision which is wealthier AFTER i have preyed on the current one?
        -- TODO: this is not very well specified in the SugarScape book. we don't know the vision of the other agent, and its information we should not have access to
        vulnerableToRetaliation :: Double -> SugarScapeAgentOut -> Bool
        vulnerableToRetaliation payoff a = (not . null) retaliatingCells
            where
                sugarLevelAgent = sugAgSugarLevel $ aoState a
                futureSugarLevel = (payoff + sugarLevelAgent)

                cellsInSight = agentLookout a
                retaliatingCells = filter (filterTargetCell (occupierRetaliator futureSugarLevel myTribe)) cellsInSight

        moveAndHarvestBestCell :: ((EnvCoord, SugarScapeEnvCell), Double) -> SugarScapeAgentOut -> SugarScapeAgentOut
        moveAndHarvestBestCell ((cellCoord, cell), payoff) a 
            | cellOccupied cell = killOccupierOfCell a'' cell
            | otherwise = a''
                where
                    sugarLevelAgent = sugAgSugarLevel $ aoState a
                    newSugarLevelAgent = (payoff + sugarLevelAgent)

                    a' = unoccupyPosition $ updateDomainState (\s -> s { sugAgSugarLevel = newSugarLevelAgent }) a 

                    cellHarvestedAndOccupied = cell {
                            sugEnvSugarLevel = 0.0,
                            sugEnvOccupier = Just (cellOccupier (aoId a') (aoState a')),
                            sugEnvPolutionLevel = 0
                    }
                            
                    env = changeCellAt cellCoord cellHarvestedAndOccupied (aoEnv a') 
                    a'' = a' { aoEnvPos = cellCoord, aoEnv = env }

        killOccupierOfCell :: SugarScapeAgentOut -> SugarScapeEnvCell -> SugarScapeAgentOut
        killOccupierOfCell a cell = sendMessage (occupierId, KilledInCombat) a 
            where
                occupier = fromJust $ sugEnvOccupier cell
                occupierId = sugEnvOccId occupier 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter IV: Sugar and Spice - Trade Comes to the Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentTrading :: SugarScapeAgentOut -> SugarScapeAgentOut
agentTrading a = agentTradingConversation nids a
    where
        nids = neighbourIds a

        agentTradingConversation :: [AgentId]
                                    -> SugarScapeAgentOut
                                    -> SugarScapeAgentOut
        agentTradingConversation [] a = conversationEnd a
        agentTradingConversation (receiverId:otherAis) a = conversation (receiverId, m) agentTradingConversationsReply a 
            where
                mrsSelf = agentMRS $ aoState a
                m = TradingOffer mrsSelf

                agentTradingConversationsReply :: SugarScapeAgentConversationSender
                agentTradingConversationsReply a Nothing = agentTradingConversation otherAis a 
                agentTradingConversationsReply a (Just (_, TradingRefuse)) = agentTradingConversation otherAis a
                agentTradingConversationsReply a (Just (_, (TradingTransact _))) = agentTradingConversation otherAis a -- NOTE: other agent has transacted, continue with next
                agentTradingConversationsReply a (Just (senderId, (TradingAccept mrsOther))) 
                    | welfareIncreases = conversation (senderId, TradingTransact mrsSelf) agentTradingConversationsReply aAfterTrade
                    | otherwise = agentTradingConversation otherAis a 
                    where
                        welfareIncreases = agentTradeIncreaseWelfare s mrsOther
                        s = aoState a
                        s' = agentTradeExchange s mrsOther
                        aAfterTrade = a { aoState = s' }

-- NOTE: we ignore cross-over trades which is forbidden in the SugarScape-Book. We claim in our implementation it is not a problem as it works different.
--       also agents move on in the next step and won't be neighbours anyway, so a cross-over would not really become a problem in a way as Epstein and Axtell said it would create infinite recursion
--       which probably would occur in their oo-implementation because of direct method-calls
handleTradingOffer :: Double
                        -> SugarScapeAgentIn
                        -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingOffer mrsOther ain 
    | welfareIncreases = (TradingAccept mrsSelf, ain)     -- This makes the agent better off
    | otherwise = (TradingRefuse, ain)                      -- This trade would make the agent worse off, refuse the trade
    where
        s = aiState ain
        mrsSelf = agentMRS s
        welfareIncreases = agentTradeIncreaseWelfare s mrsOther

handleTradingTransact :: Double
                            -> SugarScapeAgentIn
                            -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingTransact mrsOther ain = (TradingTransact mrsOther, ainAfterTrade) -- NOTE: simply reply with the same transaction-message
    where
        s = aiState ain
        s' = agentTradeExchange s mrsOther
        ainAfterTrade = ain { aiState = s' }

agentCredit :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCredit ain a = agentRequestCredit $ agentCheckCreditPaybackDue $ agentCreditPaybackIncoming ain $ agentCreditDeathIncoming ain a

-- NOTE: for now only sugar is lended & borrowed, no spice
agentRequestCredit :: SugarScapeAgentOut -> SugarScapeAgentOut
agentRequestCredit a
    | hasNeighbours = agentCreditConversation nids a
    | otherwise = a
    where
        s = aoState a
        nids = neighbourIds a
        hasNeighbours = (not $ null nids)

        agentCreditConversation :: [AgentId]
                                    -> SugarScapeAgentOut
                                    -> SugarScapeAgentOut
        agentCreditConversation [] a = conversationEnd a
        agentCreditConversation (receiverId:otherAis) a 
            | isPotentialBorrower s = conversation (receiverId, CreditRequest) agentCreditConversationsReply a 
            | otherwise = conversationEnd a
            where
                agentCreditConversationsReply :: SugarScapeAgentConversationSender
                agentCreditConversationsReply a Nothing = agentCreditConversation otherAis a
                agentCreditConversationsReply a (Just (_, CreditRequestRefuse)) = agentCreditConversation otherAis a 
                agentCreditConversationsReply a (Just (lenderId, CreditOffer credit)) = agentCreditConversation otherAis aAfterBorrowing
                    where
                        s = aoState a
                        age = sugAgAge s

                        (faceValue, creditDuration, creditInterestRate) = credit
                        creditDueAge = age + creditDuration 

                        creditInfo = (lenderId, creditDueAge, credit)
                        aAfterBorrowing = updateDomainState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + faceValue,
                                                                    sugAgBorrowingCredits = creditInfo : sugAgBorrowingCredits s }) a 

-- NOTE: if a borrower dies: notify the lenders so they know they take a loss (remove it from open credits)
-- NOTE: if a lender dies: notify the borrowers so they know they don't have to pay back
-- NOTE that we don't implement the inheritance-rule for loans
agentDeathHandleCredits :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDeathHandleCredits a = aNotifiedBorrowers
    where
        s = aoState a
        lenderIds = map (\(lid, _, _) -> lid) (sugAgBorrowingCredits s)
        borrowerIds = sugAgLendingCredits s
        
        aNotifiedLenders = broadcastMessage CreditBorrowerDied lenderIds a 
        aNotifiedBorrowers = broadcastMessage CreditLenderDied borrowerIds aNotifiedLenders

agentCreditDeathIncoming :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCreditDeathIncoming ain a = onMessage creditDeathAction ain a
    where
        creditDeathAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        creditDeathAction a (borrowerId, CreditBorrowerDied) = borrowerDied a borrowerId
        creditDeathAction a (lenderId, CreditLenderDied) = lenderDied a lenderId
        creditDeathAction a _ = a

        -- NOTE: the borrower could have borrowed multiple times from this lender, remove ALL ids
        borrowerDied :: SugarScapeAgentOut -> AgentId -> SugarScapeAgentOut
        borrowerDied a borrowerId = a' 
            where
                s = aoState a
                borrowers = sugAgLendingCredits s
                borrowersRemoved = filter (/=borrowerId) borrowers
                a' = updateDomainState (\s -> s { sugAgLendingCredits = borrowersRemoved } ) a 

        -- NOTE: the lender could have lended multiple times to this borrower, remove ALL credits
        lenderDied :: SugarScapeAgentOut -> AgentId -> SugarScapeAgentOut
        lenderDied a lenderId = a' 
            where
                s = aoState a
                borrowedCredits = sugAgBorrowingCredits s
                borrowersRemoved = filter (\(lId, _, _) -> lId /= lenderId) borrowedCredits
                a' = updateDomainState (\s -> s { sugAgBorrowingCredits = borrowersRemoved } ) a 

agentCreditPaybackIncoming :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCreditPaybackIncoming ain a = onMessage creditPaybackAction ain a
    where
        creditPaybackAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        creditPaybackAction a (_, (CreditPaybackHalf amount)) = halfCreditPayback a amount
        creditPaybackAction a (borrowerId, (CreditPaybackFull amount)) = fullCreditPayback a borrowerId amount
        creditPaybackAction a _ = a

        -- NOTE: in this case we don't remove the borrower because it has not yet payed back the whole credit
        halfCreditPayback :: SugarScapeAgentOut -> Double -> SugarScapeAgentOut
        halfCreditPayback = agentChangeSugarWealth 

        -- NOTE: in this case we just remove the first borrower-id we find. It is possible that this lender has lended multiple times to the borrower but this doesnt matter in this case
        fullCreditPayback :: SugarScapeAgentOut -> AgentId -> Double -> SugarScapeAgentOut
        fullCreditPayback a borrowerId amount = agentChangeSugarWealth a' amount
            where
                s = aoState a
                borrowers = sugAgLendingCredits s
                borrowersFirstRemoved = delete borrowerId borrowers
                a' = updateDomainState (\s -> s { sugAgLendingCredits = borrowersFirstRemoved } ) a 

agentChangeSugarWealth :: SugarScapeAgentOut -> Double -> SugarScapeAgentOut
agentChangeSugarWealth a amount = updateDomainState (\s -> s { sugAgSugarLevel = newWealth } ) a 
    where
        s = aoState a
        wealth = sugAgSugarLevel s 
        newWealth = wealth + amount

agentCheckCreditPaybackDue :: SugarScapeAgentOut -> SugarScapeAgentOut
agentCheckCreditPaybackDue a = aAfterPayback
    where
        s = aoState a
        borrowingCredits = sugAgBorrowingCredits s 

        (a', borrowingCredits') = foldr agentCheckCreditPaybackAux (a, []) borrowingCredits
        aAfterPayback = updateDomainState (\s -> s { sugAgBorrowingCredits = borrowingCredits'}) a' 

        agentCheckCreditPaybackAux :: SugarScapeCreditInfo -> (SugarScapeAgentOut, [SugarScapeCreditInfo]) -> (SugarScapeAgentOut, [SugarScapeCreditInfo])
        agentCheckCreditPaybackAux creditInfo@(lenderId, ageDue, credit) (a, accCredits) 
            | creditDue = paybackCredit --(a, accCredits)
            | otherwise = (a, creditInfo : accCredits)
            where
                s = aoState a
                age = sugAgAge s
                creditDue = ageDue >= age

                paybackCredit :: (SugarScapeAgentOut, [SugarScapeCreditInfo])
                paybackCredit 
                    | fullPaybackPossible = (aAfterPayback, accCredits)
                    | otherwise = (aAfterPayback, newCreditInfo : accCredits)
                    where
                        (faceValue, creditDuration, creditInterestRate) = credit

                        wealth = sugAgSugarLevel s
                        dueAmount = faceValue + (faceValue * (creditInterestRate / 100))
                        fullPaybackPossible = wealth >= dueAmount

                        paybackAmount = if fullPaybackPossible then dueAmount else wealth * 0.5
                        paybackMessage = if fullPaybackPossible then (CreditPaybackFull paybackAmount) else (CreditPaybackHalf paybackAmount)

                        newCredit = (faceValue - paybackAmount, creditDuration, creditInterestRate)
                        newCreditInfo = (lenderId, age + creditDuration, newCredit)

                        aReducedWealth = agentChangeSugarWealth a paybackAmount
                        aAfterPayback = sendMessage (lenderId, paybackMessage) aReducedWealth

handleCreditRequest :: SugarScapeAgentIn -> AgentId -> (SugarScapeMsg, SugarScapeAgentIn)
handleCreditRequest ain borrowerId
    | isLender = (CreditOffer credit, ainAfterCreditOffer)
    | otherwise = (CreditRequestRefuse, ain)
    where
        mayFaceValue = potentialLender s
        isLender = isJust mayFaceValue
        
        faceValue = fromJust mayFaceValue
        credit = (faceValue, lendingCreditDuration, lendingCreditInterestRate)

        s = aiState ain
        s' = s { sugAgSugarLevel = (sugAgSugarLevel s) - faceValue,
                sugAgLendingCredits = borrowerId : (sugAgLendingCredits s) }
        ainAfterCreditOffer = ain { aiState = s' }

-- NOTE: haven't implemented "On the Evolution of Foresight"
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter V: Disease Processes
------------------------------------------------------------------------------------------------------------------------
agentDiseaseContact :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentDiseaseContact ain a = onMessage diseaseContactAction ain a
    where
        diseaseContactAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        diseaseContactAction a (_, (DiseaseContact d)) = updateDomainState (\s -> s { sugAgDiseases = d : (sugAgDiseases s) } ) a 
        diseaseContactAction a _ = a

agentDiseasesTransmit :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDiseasesTransmit a  
    | (isDiseased s) && hasNeighbours = sendMessages msgs a 
    | otherwise = a
    where
        s = aoState a
        nids = neighbourIds a
        hasNeighbours = not $ null nids

        neighbourCount = length nids
        diseases = sugAgDiseases $ aoState a
        (randDisease, a') = agentPickRandomMultiple diseases neighbourCount a 
        msgs = map (\(receiverId, disease) -> (receiverId, DiseaseContact disease)) (zip nids randDisease)

agentImmunize :: SugarScapeAgentOut -> SugarScapeAgentOut
agentImmunize a = updateDomainState (\s -> s { sugAgImmuneSys = immuneSystem', sugAgDiseases = diseases' }) a 
    where
        s = aoState a
        immuneSystem = sugAgImmuneSys s
        diseases = sugAgDiseases s

        (immuneSystem', diseases') = foldr agentImmunizeAux (immuneSystem, []) diseases

agentDiseaseProcesses :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentDiseaseProcesses ain a = agentImmunize $ agentDiseasesTransmit $ agentDiseaseContact ain a
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- CONVERSATION-HANDLER
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentConversationPure :: SugarScapeAgentConversation
sugarScapeAgentConversationPure ain (_, (MatingRequest tup)) = Just $ handleMatingConversation tup ain
sugarScapeAgentConversationPure ain (_, (MatingChild childId)) = Just (MatingChildAck, ain')
    where
        s = aiState ain
        s' = s { sugAgChildren = childId : (sugAgChildren s)}
        ain' = ain { aiState = s' }

sugarScapeAgentConversationPure ain (_, (TradingOffer mrs)) = Just $ handleTradingOffer mrs ain
sugarScapeAgentConversationPure ain (_, (TradingTransact mrs)) = Just $ handleTradingTransact mrs ain

sugarScapeAgentConversationPure ain (borrowerId, CreditRequest) = Just $ handleCreditRequest ain borrowerId

sugarScapeAgentConversationPure _ _ = Nothing
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- BEHAVIOUR-CONTROL
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviourFunc :: Double -> SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut 
sugarScapeAgentBehaviourFunc age ain a = 
    do     
        let a0 = agentKilledInCombat ain a 
        if isDead a0 then
            agentDeathHandleCredits a0
            else
                do
                    let a1 = agentAgeing age a0
                    if isDead a1 then
                        agentDeathHandleCredits a1
                        else
                            do
                                let a2 = agentMetabolism a1
                                if isDead a2 then
                                    agentDeathHandleCredits a2
                                    else 
                                        do
                                            let a3 = agentNonCombatMove a2
                                            let a4 = inheritSugar ain a3
                                            -- let a5 = agentCultureContact ain a4
                                            -- let a6 = agentSex ain a5
                                            -- let a7 = agentTrading a5
                                            let a8 = agentCredit ain a4
                                            --let a9 = agentDiseaseProcesses ain a4
                                            a8
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviourPure :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviourPure = agentPure sugarScapeAgentBehaviourFunc
------------------------------------------------------------------------------------------------------------------------