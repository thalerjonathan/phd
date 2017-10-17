module SugarScape.Agent (
    sugarScapeAgentConversation,
    sugarScapeAgentBehaviour
  ) where

import SugarScape.Common
import SugarScape.Model
import SugarScape.Environment

import FRP.FrABS

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.State
------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR MONADIC 
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentCellOnCoordM :: SugarScapeEnvironment -> State SugarScapeAgentOut (Discrete2dCoord, SugarScapeEnvCell)
agentCellOnCoordM e = 
    do
        coord <- domainStateFieldM sugAgCoord
        let cell = cellAt coord e
        return (coord, cell)

agentDiesM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentDiesM e = unoccupyPositionM e >>= (\e' -> killM >> return e')

unoccupyPositionM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
unoccupyPositionM e = 
    do
        (coord, cell) <- agentCellOnCoordM e
        let cellUnoccupied = cell { sugEnvOccupier = Nothing }
        return $ changeCellAt coord cellUnoccupied e

passWealthOnM :: State SugarScapeAgentOut ()
passWealthOnM
    | _enableInheritance_ =
        do
            sugarLevel <- domainStateFieldM sugAgSugarLevel
            childrenIds <- domainStateFieldM sugAgChildren

            let hasChildren = (not . null) childrenIds

            when hasChildren $
                do 
                    let childrenCount = length childrenIds
                    let childrenSugarShare = sugarLevel / fromIntegral childrenCount
                    broadcastMessageM (InheritSugar childrenSugarShare) childrenIds
    | otherwise = return ()

starvedToDeathM :: State SugarScapeAgentOut Bool
starvedToDeathM = 
    do
        sugar <- domainStateFieldM sugAgSugarLevel
        spice <- domainStateFieldM sugAgSpiceLevel

        if _enableSpice_ then
            return $ (sugar <= 0) || (spice <= 0)
            else
                return $ sugar <= 0

agentMetabolismM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMetabolismM e =
    do
        s <- getDomainStateM
        aid <- agentIdM
        let (sugarMetab, spiceMetab) = metabolismAmount s

        sugarLevel <- domainStateFieldM sugAgSugarLevel
        spiceLevel <- domainStateFieldM sugAgSpiceLevel

        let newSugarLevel = max 0 (sugarLevel - sugarMetab)
        let newSpiceLevel = max 0 (spiceLevel - spiceMetab)

        updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevel, sugAgSpiceLevel = newSpiceLevel })

        -- NOTE: for now the metabolism (and harvest) of spice does not cause any polution
        coord <- domainStateFieldM sugAgCoord
        let e' = poluteCell (sugarMetab * polutionMetabolismFactor) coord e

        ifThenElseM
            starvedToDeathM
            (agentDiesM e')
            (return e')

agentNonCombatMoveM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentNonCombatMoveM e = 
    do
        cellsInSight <- agentLookoutM e
        coord <- domainStateFieldM sugAgCoord

        let unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

        ifThenElse 
            (null unoccupiedCells)
            (agentStayAndHarvestM e)
            (do
                -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
                --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
                let selfCell = cellAt coord e
                let unoccupiedCells' = (coord, selfCell) : unoccupiedCells

                let bf = bestCellFunc
                let bestCells = selectBestCells bf coord unoccupiedCells'
                (cellCoord, _) <- agentRandomPickM bestCells
                agentMoveAndHarvestCellM cellCoord e)

agentLookoutM :: SugarScapeEnvironment -> State SugarScapeAgentOut [(Discrete2dCoord, SugarScapeEnvCell)]
agentLookoutM e = 
    do
        vis <- domainStateFieldM sugAgVision
        coord <- domainStateFieldM sugAgCoord
        return $ neighboursInNeumannDistance coord vis False e

agentStayAndHarvestM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentStayAndHarvestM e = domainStateFieldM sugAgCoord >>= (\coord -> agentHarvestCellM coord e)

agentMoveAndHarvestCellM :: Discrete2dCoord -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMoveAndHarvestCellM cellCoord e = agentHarvestCellM cellCoord e >>= (\e' -> agentMoveToM cellCoord e')

agentMoveToM :: Discrete2dCoord -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMoveToM cellCoord e = 
    do
        e' <- unoccupyPositionM e

        updateDomainStateM (\s -> s { sugAgCoord = cellCoord })

        s <- getDomainStateM
        aid <- agentIdM

        let cell = cellAt cellCoord e'
        let cellOccupied = cell { sugEnvOccupier = Just (cellOccupier aid s) }
        return $ changeCellAt cellCoord cellOccupied e'

agentHarvestCellM :: Discrete2dCoord -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentHarvestCellM cellCoord e = 
    do
        let cell = cellAt cellCoord e

        sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
        spiceLevelAgent <- domainStateFieldM sugAgSpiceLevel

        let sugarLevelCell = sugEnvSugarLevel cell
        let spiceLevelCell = sugEnvSpiceLevel cell

        let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent
        let newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

        updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgSpiceLevel = newSpiceLevelAgent })

        let cellHarvested = cell { sugEnvSugarLevel = 0.0, sugEnvSpiceLevel = 0.0 }
        let e' = changeCellAt cellCoord cellHarvested e
       
        -- NOTE: at the moment harvesting SPICE does not influence the polution
        return $ poluteCell (sugarLevelCell * polutionHarvestFactor ) cellCoord e'

agentAgeingM :: Double -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentAgeingM newAge e =
    do
        updateDomainStateM (\s -> s { sugAgAge = newAge })

        ifThenElseM
            dieFromAgeM
            (do
                birthNewAgentM e
                passWealthOnM
                agentDiesM e)
            (return e)

birthNewAgentM :: SugarScapeEnvironment -> State SugarScapeAgentOut ()
birthNewAgentM e
    | not _enableBirthAgentOnAgeDeath_ = return ()
    | otherwise =
        do
            newAgentId <- agentIdM -- NOTE: in this case we keep the old id
            newAgentCoord <- findUnoccpiedRandomPositionM   -- NOTE: why not take the same position?
            newAgentDef <- agentRandomM $ randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviour sugarScapeAgentConversationM
            createAgentM newAgentDef

        where
            findUnoccpiedRandomPositionM :: State SugarScapeAgentOut Discrete2dCoord
            findUnoccpiedRandomPositionM =
                do
                    (c, coord) <- agentRandomM $ randomCell e
                    ifThenElse
                        (cellOccupied c) 
                        findUnoccpiedRandomPositionM 
                        (return coord)

dieFromAgeM :: State SugarScapeAgentOut Bool
dieFromAgeM = 
    do
        age <- domainStateFieldM sugAgAge
        maxAge <- domainStateFieldM sugAgMaxAge
        return $ age > maxAge

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSexM :: SugarScapeAgentIn -> SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentSexM ain e 
    | not _enableSex_ = return ()
    | otherwise =
        whenM 
            isFertileM
                (do
                    coord <- domainStateFieldM sugAgCoord
                    nids <- neighbourIdsM e
                    let unncs = map fst (unoccupiedNeighbourhoodOfNeighbours coord e)
                    agentMatingConversationM ain nids unncs)

agentMatingConversationM :: SugarScapeAgentIn 
                            -> [AgentId]
                            -> [Discrete2dCoord]
                            -> State SugarScapeAgentOut ()
agentMatingConversationM _ [] _ = conversationEndM
agentMatingConversationM _ _ [] = conversationEndM
agentMatingConversationM ain (receiverId:ais) allCs@(coord:cs) =
    do
        gender <- domainStateFieldM sugAgGender

        ifThenElseM
            satisfiesWealthForChildBearingM
            (conversationM 
                (receiverId, MatingRequest gender) 
                (conversationReplyMonadicRunner agentMatingConversationsReplyM))
            conversationEndM

    where
        agentMatingConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg) 
                                            -> SugarScapeEnvironment
                                            -> State SugarScapeAgentOut SugarScapeEnvironment
        agentMatingConversationsReplyM Nothing e = agentMatingConversationM ain ais allCs >> return e  -- NOTE: the target was not found or does not have a handler, continue with the next
        agentMatingConversationsReplyM (Just (_, MatingReplyNo)) e = agentMatingConversationM ain ais allCs >> return e
        agentMatingConversationsReplyM (Just (senderId, MatingReplyYes otherTup)) e = mate otherTup e
        agentMatingConversationsReplyM (Just _) e = agentMatingConversationM ain ais allCs >> return e  -- NOTE: unexpected/MatingChildAck reply, continue with the next

        mate :: MatingReplyTuple
                -> SugarScapeEnvironment 
                -> State SugarScapeAgentOut SugarScapeEnvironment
        mate otherTup e =
            do
                initialSugarEndow <- domainStateFieldM sugAgSugarInit

                let mySugarContribution = initialSugarEndow * 0.5
                mySugarMetab <- domainStateFieldM sugAgSugarMetab
                mySpiceMetab <- domainStateFieldM sugAgSpiceMetab
                myVision <- domainStateFieldM sugAgVision
                myCulturalTag <- domainStateFieldM sugAgCulturalTag
                myImmuneSysBorn <- domainStateFieldM sugAgImmuneSysBorn

                let newBornId = nextAgentId ain

                newBornDef <- 
                    agentRandomM
                        (createNewBorn 
                            (newBornId, coord)
                            (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn)
                            otherTup
                            sugarScapeAgentBehaviour
                            sugarScapeAgentConversationM)

                let e' = updateCellAt coord (\c -> c { sugEnvOccupier = Just (cellOccupier newBornId (adState newBornDef)) }) e
                
                updateDomainStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s - mySugarContribution,
                                              sugAgChildren = newBornId : sugAgChildren s})
                
                createAgentM newBornDef

                conversationM 
                    (receiverId, MatingChild newBornId)
                    (conversationIgnoreReplyMonadicRunner $ agentMatingConversationM ain ais cs) -- NOTE: ignore the incoming environment because we just want to continue without being actually interested in the 

                return e'

handleMatingConversationM :: SugarScapeAgentGender
                                -> SugarScapeAgentIn
                                -> (SugarScapeMsg, SugarScapeAgentIn)
handleMatingConversationM otherGender ain 
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

inheritSugarM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
inheritSugarM ain = onMessageMState inheritSugarActionM ain
    where
        inheritSugarActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        inheritSugarActionM (_, InheritSugar sug) = updateDomainStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sug })
        inheritSugarActionM _ = return ()

agentCultureContactM :: SugarScapeAgentIn -> SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentCultureContactM ain e = 
    do
        onMessageMState cultureContactActionM ain

        nids <- neighbourIdsM e
        culturalTag <- domainStateFieldM sugAgCulturalTag

        broadcastMessageM (CulturalContact culturalTag) nids 

    where
        cultureContactActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        cultureContactActionM (_, (CulturalContact tagActive)) = 
            do
                agentTag <- domainStateFieldM sugAgCulturalTag
                agentTag' <- agentRandomM (cultureContact tagActive agentTag)
                
                let tribe = calculateTribe agentTag'

                updateDomainStateM (\s -> s { sugAgCulturalTag = agentTag',
                                              sugAgTribe = tribe})
        cultureContactActionM _ = return ()

agentKilledInCombatM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentKilledInCombatM ain = onMessageMState killedInCombatActionM ain
    where
        killedInCombatActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        killedInCombatActionM (_, KilledInCombat) = killM -- NOTE: don't unoccupie position (as in agentdies) because it is occupied by the killer already
        killedInCombatActionM _ = return ()

agentMoveM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMoveM e
    | _enableCombat_ = agentCombatMoveM e
    | otherwise = agentNonCombatMoveM e

agentCombatMoveM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentCombatMoveM e =
    do
        cellsInSight <- agentLookoutM e
        myTribe <- domainStateFieldM sugAgTribe
        myWealth <- domainStateFieldM sugAgSugarLevel 

        let targetCells = filterOccupiers (occupierCombatable myWealth myTribe) cellsInSight
        
        ifThenElse 
            (null targetCells)
            (agentNonCombatMoveM e)--(agentStayAndHarvestM e)
            (do
                coord <- domainStateFieldM sugAgCoord

                -- TODO: refactor this into common function (searching for the best)?
                let targeCellsWithPayoff = map cellPayoff targetCells

                let cellsSortedByPayoff = sortBy (\c1 c2 -> compare (snd c2) (snd c1)) targeCellsWithPayoff
                let bestCellPayoff = snd $ head cellsSortedByPayoff
                let bestCells = filter ((==bestCellPayoff) . snd) cellsSortedByPayoff

                let shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattanDisc2d coord (fst . fst $ c1)) (distanceManhattanDisc2d coord (fst . fst $ c2))) bestCells
                let shortestdistanceManhattan = distanceManhattanDisc2d coord (fst . fst $ head shortestdistanceManhattanBestCells)
                let bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattanDisc2d coord) . fst . fst) shortestdistanceManhattanBestCells

                bestCell@((_,_), payoff) <- agentRandomPickM bestShortestdistanceManhattanCells
                
                ifThenElseM 
                    (vulnerableToRetaliationM payoff e)
                    (agentNonCombatMoveM e)-- (agentStayAndHarvestM e)
                    (moveAndHarvestAndKillM bestCell e)
            )

    where
        -- NOTE: calculate if retalion is possible: is there an agent of the other tribe in my vision which is wealthier AFTER i have preyed on the current one?
        --       this is not very well specified in the SugarScape book. we don't know the vision of the other agent, and it is information we should not have access to
        --       but it reads in the book like the vision of the other agent does not matter, it is just a question of one owns vision
        vulnerableToRetaliationM :: Double -> SugarScapeEnvironment -> State SugarScapeAgentOut Bool
        vulnerableToRetaliationM payoff e =
            do
                sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
                cellsInSight <- agentLookoutM e
                myTribe <- domainStateFieldM sugAgTribe

                let futureSugarLevel = (payoff + sugarLevelAgent)
                let retaliatingCells = filterOccupiers (occupierRetaliator futureSugarLevel myTribe) cellsInSight

                return $ (not . null) retaliatingCells

        moveAndHarvestAndKillM :: ((Discrete2dCoord, SugarScapeEnvCell), Double) 
                                    -> SugarScapeEnvironment 
                                    -> State SugarScapeAgentOut SugarScapeEnvironment
        moveAndHarvestAndKillM ((cellCoord, cell), payoff) e =
            do
                sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
                let newSugarLevelAgent = (payoff + sugarLevelAgent)

                e' <- unoccupyPositionM e
                updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgCoord = cellCoord })

                aid <- agentIdM
                s <- getDomainStateM

                let cellHarvestedAndOccupied = cell {
                        sugEnvSugarLevel = 0.0,
                        sugEnvOccupier = Just (cellOccupier aid s),
                        sugEnvPolutionLevel = 0
                }

                let e'' = changeCellAt cellCoord cellHarvestedAndOccupied e'

                when 
                    (cellOccupied cell) 
                        (killOccupierOfCellM cell)

                return e''

        killOccupierOfCellM :: SugarScapeEnvCell -> State SugarScapeAgentOut ()
        killOccupierOfCellM cell = sendMessageM (occupierId, KilledInCombat)
            where
                occupier = fromJust $ sugEnvOccupier cell
                occupierId = sugEnvOccId occupier 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter IV: Sugar and Spice - Trade Comes to the Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentTradingM :: SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentTradingM e 
    | _enableTrading_ = neighbourIdsM e >>= agentTradingConversationM
    | otherwise = return ()
    where
        agentTradingConversationM :: [AgentId]
                                    -> State SugarScapeAgentOut ()
        agentTradingConversationM [] = conversationEndM
        agentTradingConversationM (receiverId:otherAis) = 
            do
                s <- getDomainStateM
                let mrsSelf = agentMRS s
                conversationM 
                    (receiverId, (TradingOffer mrsSelf)) 
                    (conversationIgnoreEnvReplyMonadicRunner $ agentTradingConversationsReplyM mrsSelf)
            
            where
                agentTradingConversationsReplyM :: Double
                                                    -> Maybe (AgentMessage SugarScapeMsg)
                                                    -> State SugarScapeAgentOut ()
                agentTradingConversationsReplyM _ Nothing = agentTradingConversationM otherAis 
                agentTradingConversationsReplyM _ (Just (_, TradingRefuse)) = agentTradingConversationM otherAis 
                agentTradingConversationsReplyM _ (Just (_, (TradingTransact _))) = agentTradingConversationM otherAis  -- NOTE: other agent has transacted, continue with next
                agentTradingConversationsReplyM mrsSelf (Just (senderId, (TradingAccept mrsOther))) =
                    do
                        s <- getDomainStateM

                        let welfareIncreases = agentTradeIncreaseWelfare s mrsOther
                        
                        ifThenElse 
                            welfareIncreases
                            (do
                                let s' = agentTradeExchange s mrsOther
                                setDomainStateM s'
                                conversationM 
                                    (senderId, TradingTransact mrsSelf) 
                                    (conversationIgnoreEnvReplyMonadicRunner $ agentTradingConversationsReplyM mrsSelf))
                            (agentTradingConversationM otherAis)

agentCreditM :: SugarScapeAgentIn -> SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentCreditM ain e 
    | _enableCredit_ = agentRequestCreditM e >> agentCheckCreditPaybackDueM >> agentCreditPaybackIncomingM ain >> agentCreditDeathIncomingM ain
    | otherwise = return ()

-- NOTE: for now only sugar is lended & borrowed, no spice
agentRequestCreditM :: SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentRequestCreditM e =
    do
        nids <- neighbourIdsM e
        let hasNeighbours = (not $ null nids)
        when hasNeighbours (agentCreditConversationM nids)

    where
        agentCreditConversationM :: [AgentId]
                                    -> State SugarScapeAgentOut ()
        agentCreditConversationM [] = conversationEndM
        agentCreditConversationM (receiverId:otherAis) =
            do
                s <- getDomainStateM

                ifThenElse 
                    (isPotentialBorrower s)
                    (conversationM (receiverId, CreditRequest) 
                        (conversationIgnoreEnvReplyMonadicRunner agentCreditConversationsReplyM))
                    conversationEndM
            where
                agentCreditConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg)
                                                    -> State SugarScapeAgentOut ()
                agentCreditConversationsReplyM Nothing = agentCreditConversationM otherAis
                agentCreditConversationsReplyM (Just (_, CreditRequestRefuse)) = agentCreditConversationM otherAis 
                agentCreditConversationsReplyM (Just (lenderId, CreditOffer credit)) = 
                    do
                        age <- domainStateFieldM sugAgAge

                        let (faceValue, creditDuration, creditInterestRate) = credit
                        let creditDueAge = age + creditDuration 
                        let creditInfo = (lenderId, creditDueAge, credit)
                        updateDomainStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + faceValue,
                                                        sugAgBorrowingCredits = creditInfo : sugAgBorrowingCredits s })

                        agentCreditConversationM otherAis

-- NOTE: if a borrower dies: notify the lenders so they know they take a loss (remove it from open credits)
-- NOTE: if a lender dies: notify the borrowers so they know they don't have to pay back
-- NOTE that we don't implement the inheritance-rule for loans
agentDeathHandleCreditsM :: State SugarScapeAgentOut ()
agentDeathHandleCreditsM = 
    do
        borrowedCredits <- domainStateFieldM sugAgBorrowingCredits
        borrowerIds <- domainStateFieldM sugAgLendingCredits

        let lenderIds = map (\(lid, _, _) -> lid) borrowedCredits
        
        broadcastMessageM CreditBorrowerDied lenderIds
        broadcastMessageM CreditLenderDied borrowerIds

agentCreditDeathIncomingM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentCreditDeathIncomingM ain = onMessageMState creditDeathActionM ain
    where
        creditDeathActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        creditDeathActionM (borrowerId, CreditBorrowerDied) = borrowerDiedM borrowerId
        creditDeathActionM (lenderId, CreditLenderDied) = lenderDiedM lenderId
        creditDeathActionM _ = return ()

        -- NOTE: the borrower could have borrowed multiple times from this lender, remove ALL ids
        borrowerDiedM :: AgentId -> State SugarScapeAgentOut ()
        borrowerDiedM borrowerId = 
            do
                borrowers <- domainStateFieldM sugAgLendingCredits
                let borrowersRemoved = filter (/=borrowerId) borrowers
                updateDomainStateM (\s -> s { sugAgLendingCredits = borrowersRemoved } )

        -- NOTE: the lender could have lended multiple times to this borrower, remove ALL credits
        lenderDiedM :: AgentId -> State SugarScapeAgentOut ()
        lenderDiedM lenderId = 
            do
                borrowedCredits <- domainStateFieldM sugAgBorrowingCredits
                let borrowersRemoved = filter (\(lId, _, _) -> lId /= lenderId) borrowedCredits
                updateDomainStateM (\s -> s { sugAgBorrowingCredits = borrowersRemoved } )

agentCreditPaybackIncomingM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentCreditPaybackIncomingM ain = onMessageMState creditPaybackActionM ain
    where
        creditPaybackActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        creditPaybackActionM (_, (CreditPaybackHalf amount)) = halfCreditPaybackM amount
        creditPaybackActionM (borrowerId, (CreditPaybackFull amount)) = fullCreditPaybackM borrowerId amount
        creditPaybackActionM _ = return ()

        -- NOTE: in this case we don't remove the borrower because it has not yet payed back the whole credit
        halfCreditPaybackM :: Double -> State SugarScapeAgentOut ()
        halfCreditPaybackM = agentChangeSugarWealthM 

        -- NOTE: in this case we just remove the first borrower-id we find. It is possible that this lender has lended multiple times to the borrower but this doesnt matter in this case
        fullCreditPaybackM :: AgentId -> Double -> State SugarScapeAgentOut ()
        fullCreditPaybackM borrowerId amount = 
            do
                borrowers <- domainStateFieldM sugAgLendingCredits
                let borrowersFirstRemoved = delete borrowerId borrowers
                updateDomainStateM (\s -> s { sugAgLendingCredits = borrowersFirstRemoved } )
                agentChangeSugarWealthM amount

agentChangeSugarWealthM :: Double -> State SugarScapeAgentOut ()
agentChangeSugarWealthM amount = updateDomainStateM (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + amount } )

agentCheckCreditPaybackDueM :: State SugarScapeAgentOut ()
agentCheckCreditPaybackDueM = 
    do
        borrowedCredits <- domainStateFieldM sugAgBorrowingCredits

        borrowedCredits' <- foldM agentCheckCreditPaybackAuxM [] borrowedCredits
        updateDomainStateM (\s -> s { sugAgBorrowingCredits = borrowedCredits'})

    where
        agentCheckCreditPaybackAuxM :: [SugarScapeCreditInfo] 
                                        -> SugarScapeCreditInfo 
                                        -> State SugarScapeAgentOut [SugarScapeCreditInfo]
        agentCheckCreditPaybackAuxM accCredits creditInfo@(lenderId, ageDue, credit) =
            do
                age <- domainStateFieldM sugAgAge
                let creditDue = ageDue >= age

                ifThenElse creditDue
                    (paybackCredit age)
                    (return $ creditInfo : accCredits)

            where
                paybackCredit :: Double -> State SugarScapeAgentOut [SugarScapeCreditInfo]
                paybackCredit age =
                    do
                        wealth <- domainStateFieldM sugAgSugarLevel

                        let (faceValue, creditDuration, creditInterestRate) = credit
                        let dueAmount = faceValue + (faceValue * (creditInterestRate / 100))
                        let fullPaybackPossible = wealth >= dueAmount
                        
                        let paybackAmount = if fullPaybackPossible then dueAmount else wealth * 0.5
                        let paybackMessage = if fullPaybackPossible then (CreditPaybackFull paybackAmount) else (CreditPaybackHalf paybackAmount)

                        let newCredit = (faceValue - paybackAmount, creditDuration, creditInterestRate)
                        let newCreditInfo = (lenderId, age + creditDuration, newCredit)

                        agentChangeSugarWealthM paybackAmount
                        sendMessageM (lenderId, paybackMessage)

                        ifThenElse fullPaybackPossible
                            (return accCredits)
                            (return $ newCreditInfo : accCredits)

-- NOTE: we ignore cross-over trades which is forbidden in the SugarScape-Book. We claim in our implementation it is not a problem as it works different.
--       also agents move on in the next step and won't be neighbours anyway, so a cross-over would not really become a problem in a way as Epstein and Axtell said it would create infinite recursion
--       which probably would occur in their oo-implementation because of direct method-calls
-- TODO: monadic-refactoring
handleTradingOfferM :: Double
                        -> SugarScapeAgentIn
                        -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingOfferM mrsOther ain 
    | welfareIncreases = (TradingAccept mrsSelf, ain)     -- This makes the agent better off
    | otherwise = (TradingRefuse, ain)                      -- This trade would make the agent worse off, refuse the trade
    where
        s = aiState ain
        mrsSelf = agentMRS s
        welfareIncreases = agentTradeIncreaseWelfare s mrsOther

-- TODO: monadic-refactoring
handleTradingTransactM :: Double
                            -> SugarScapeAgentIn
                            -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingTransactM mrsOther ain = (TradingTransact mrsOther, ainAfterTrade) -- NOTE: simply reply with the same transaction-message
    where
        s = aiState ain
        s' = agentTradeExchange s mrsOther
        ainAfterTrade = ain { aiState = s' }

-- TODO: monadic-refactoring
handleCreditRequestM :: SugarScapeAgentIn -> AgentId -> (SugarScapeMsg, SugarScapeAgentIn)
handleCreditRequestM ain borrowerId
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
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter V: Disease Processes
------------------------------------------------------------------------------------------------------------------------
agentDiseaseContactM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentDiseaseContactM ain = onMessageMState diseaseContactActionM ain
    where
        diseaseContactActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        diseaseContactActionM (_, (DiseaseContact d)) = updateDomainStateM (\s -> s { sugAgDiseases = d : (sugAgDiseases s) } )
        diseaseContactActionM _ = return ()

agentDiseasesTransmitM :: SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentDiseasesTransmitM e =
    do
        diseases <- domainStateFieldM sugAgDiseases
        nids <- neighbourIdsM e

        let hasDiseases = (not . null) diseases
        let hasNeighbours = (not . null) nids

        when
            (hasDiseases && hasNeighbours)
            (do
                randDisease <- agentRandomPicksM diseases (length nids)
                let msgs = map (\(receiverId, disease) -> (receiverId, DiseaseContact disease)) (zip nids randDisease)
                sendMessagesM msgs)

agentImmunizeM :: State SugarScapeAgentOut ()
agentImmunizeM =
    do
        immuneSystem <- domainStateFieldM sugAgImmuneSys
        diseases <- domainStateFieldM sugAgDiseases

        let (immuneSystem', diseases') = foldr agentImmunizeAux (immuneSystem, []) diseases

        updateDomainStateM (\s -> s { sugAgImmuneSys = immuneSystem',
                                      sugAgDiseases = diseases' })

agentDiseaseProcessesM :: SugarScapeAgentIn -> SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentDiseaseProcessesM ain e = agentDiseaseContactM ain >> agentDiseasesTransmitM e >> agentImmunizeM
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CONVERSATION-HANDLER
------------------------------------------------------------------------------------------------------------------------
-- TODO: monadic-refactoring
sugarScapeAgentConversationM :: SugarScapeAgentConversation
sugarScapeAgentConversationM ain e (_, (MatingRequest tup)) = Just (m, ain', e)
    where
        (m, ain') = handleMatingConversationM tup ain
sugarScapeAgentConversationM ain e (_, (MatingChild childId)) = Just (MatingChildAck, ain', e)
    where
        s = aiState ain
        s' = s { sugAgChildren = childId : (sugAgChildren s)}
        ain' = ain { aiState = s' }
sugarScapeAgentConversationM ain e (_, (TradingOffer mrs)) = Just (m, ain', e)
    where
        (m, ain') = handleTradingOfferM mrs ain
sugarScapeAgentConversationM ain e (_, (TradingTransact mrs)) = Just (m, ain', e)
    where
        (m, ain') = handleTradingTransactM mrs ain
sugarScapeAgentConversationM ain e (borrowerId, CreditRequest) = Just (m, ain', e)
    where
        (m, ain') = handleCreditRequestM ain borrowerId
sugarScapeAgentConversationM _ _ _ = Nothing
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- BEHAVIOUR-CONTROL
-- NOTE: although we could configure each chapter separately we also provide separate chapter-functions which never call given functions
------------------------------------------------------------------------------------------------------------------------
chapterII :: SugarScapeEnvironment 
                -> Double 
                -> SugarScapeAgentIn 
                -> State SugarScapeAgentOut SugarScapeEnvironment
chapterII e age ain =
    do     
        e0 <- agentAgeingM age e
        ifThenElseM 
            isDeadM
            (return e0)
            $ do
                e1 <- agentMetabolismM e0
                ifThenElseM 
                    isDeadM
                    (return e1)
                    (agentMoveM e1)

chapterIII :: SugarScapeEnvironment 
                -> Double 
                -> SugarScapeAgentIn 
                -> State SugarScapeAgentOut SugarScapeEnvironment
chapterIII e age ain =
    do
        agentKilledInCombatM ain
        ifThenElseM
            isDeadM
            (return e)
            $ do
                e0 <- agentAgeingM age e
                ifThenElseM 
                    isDeadM
                    (return e0)
                    $ do
                        e1 <- agentMetabolismM e0
                        ifThenElseM 
                            isDeadM
                            (return e1)
                            $ do
                                e2 <- agentMoveM e1
                                agentSexM ain e2
                                inheritSugarM ain
                                agentCultureContactM ain e2
                                return e2

chapterIV :: SugarScapeEnvironment 
                -> Double 
                -> SugarScapeAgentIn 
                -> State SugarScapeAgentOut SugarScapeEnvironment
chapterIV e age ain = 
    do     
        agentKilledInCombatM ain
        ifThenElseM
            isDeadM
            (return e)
            $ do
                e0 <- agentAgeingM age e
                ifThenElseM 
                    isDeadM
                    (return e0)
                    $ do
                        e1 <- agentMetabolismM e0
                        ifThenElseM 
                            isDeadM
                            (return e1)
                            $ do
                                e2 <- agentMoveM e1
                                agentSexM ain e2
                                inheritSugarM ain
                                agentCultureContactM ain e2
                                agentTradingM e2
                                agentCreditM ain e2
                                return e2

chapterV :: SugarScapeEnvironment 
            -> Double 
            -> SugarScapeAgentIn 
            -> State SugarScapeAgentOut SugarScapeEnvironment
chapterV e age ain = 
    do     
        agentKilledInCombatM ain
        ifThenElseM
            isDeadM
            (return e)
            $ do
                e0 <- agentAgeingM age e
                ifThenElseM 
                    isDeadM
                    (return e0)
                    $ do
                        e1 <- agentMetabolismM e0
                        ifThenElseM 
                            isDeadM
                            (return e1)
                            $ do
                                e2 <- agentMoveM e1
                                agentSexM ain e2
                                inheritSugarM ain
                                agentCultureContactM ain e2
                                agentTradingM e2
                                agentCreditM ain e2
                                agentDiseaseProcessesM ain e2
                                return e2
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = agentMonadic chapterII

sugarScapeAgentConversation :: SugarScapeAgentConversation
sugarScapeAgentConversation = sugarScapeAgentConversationM
------------------------------------------------------------------------------------------------------------------------