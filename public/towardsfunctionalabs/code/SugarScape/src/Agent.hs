{-# LANGUAGE Arrows #-}
module Agent 
  (
    sugAgent
  ) where

-- import Control.Monad
-- import Control.Monad.IfElse
import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.Chimera
import FRP.BearRiver

--import Data.Maybe
--import Data.List

import Common
--import Environment
import Model

------------------------------------------------------------------------------------------------------------------------
sugAgent :: RandomGen g 
         => SugAgentState
         -> SugAgent g
sugAgent s0 aid = return sugAgentCont
  where
    sugAgentCont :: RandomGen g 
                 => SugAgentCont g
    sugAgentCont = feedback s0 (proc (ain, s) -> do
      age      <- time -< ()
      (ao, s') <- arrM (\(age, ain, s) -> lift $ runStateT (chapterII aid ain age) s) -< (age, ain, s)
      returnA -< (ao, s'))

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
chapterII :: RandomGen g
          => AgentId
          -> SugAgentIn
          -> Double
          -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
chapterII _aid _ain _age = do
  -- no lift: getting SugAgentState
  s <- get

  -- 1 lift: inside ABSMonad
  _x <- lift get
  _f <- lift $ unscheduleEventM 0
 
  -- 2 lifts: inside SugEnvironment
  _env <- lift $ lift get

  -- 3 lifts: drawing RNG
  _re <- lift $ lift $ lift $ randomExpM 1

  return $ agentOutObservable $ sugObservableFromState s 

{-
agentCellOnCoordM :: SugEnvironment -> State SugAgentOut (Discrete2dCoord, SugEnvCell)
agentCellOnCoordM e = do
  coord <- agentStateFieldM sugAgCoord
  let cell = cellAt coord e
  return (coord, cell)

agentDiesM :: SugEnvironment -> State SugAgentOut SugEnvironment
agentDiesM e = unoccupyPositionM e >>= (\e' -> killM >> return e')

unoccupyPositionM :: SugEnvironment -> State SugAgentOut SugEnvironment
unoccupyPositionM e = do
  (coord, cell) <- agentCellOnCoordM e
  let cellUnoccupied = cell { sugEnvOccupier = Nothing }
  return $ changeCellAt coord cellUnoccupied e

passWealthOnM :: State SugAgentOut ()
passWealthOnM
    | _enableInheritance_ = do
      sugarLevel <- agentStateFieldM sugAgSugarLevel
      childrenIds <- agentStateFieldM sugAgChildren

      let hasChildren = (not . null) childrenIds

      when hasChildren $ do 
        let childrenCount = length childrenIds
        let childrenSugarShare = sugarLevel / fromIntegral childrenCount
        broadcastMessageM (InheritSugar childrenSugarShare) childrenIds
    | otherwise = return ()

starvedToDeathM :: State SugAgentOut Bool
starvedToDeathM = do
  sugar <- agentStateFieldM sugAgSugarLevel
  spice <- agentStateFieldM sugAgSpiceLevel

  if _enableSpice_ 
    then return $ (sugar <= 0) || (spice <= 0)
    else return $ sugar <= 0

agentMetabolismM :: SugEnvironment -> State SugAgentOut SugEnvironment
agentMetabolismM e = do
  s <- agentStateM
  aid <- agentIdM
  let (sugarMetab, spiceMetab) = metabolismAmount s

  sugarLevel <- agentStateFieldM sugAgSugarLevel
  spiceLevel <- agentStateFieldM sugAgSpiceLevel

  let newSugarLevel = max 0 (sugarLevel - sugarMetab)
  let newSpiceLevel = max 0 (spiceLevel - spiceMetab)

  updateAgentStateM (\s -> s { sugAgSugarLevel = newSugarLevel, sugAgSpiceLevel = newSpiceLevel })

  -- NOTE: for now the metabolism (and harvest) of spice does not cause any polution
  coord <- agentStateFieldM sugAgCoord
  let e' = poluteCell (sugarMetab * polutionMetabolismFactor) coord e

  ifThenElseM
      starvedToDeathM
      (agentDiesM e')
      (return e')

agentNonCombatMoveM :: SugEnvironment -> State SugAgentOut SugEnvironment
agentNonCombatMoveM e = do
  cellsInSight <- agentLookoutM e
  coord <- agentStateFieldM sugAgCoord

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

agentLookoutM :: SugEnvironment -> State SugAgentOut [(Discrete2dCoord, SugEnvCell)]
agentLookoutM e = do
  vis <- agentStateFieldM sugAgVision
  coord <- agentStateFieldM sugAgCoord
  return $ neighboursInNeumannDistance coord vis False e

agentStayAndHarvestM :: SugEnvironment -> State SugAgentOut SugEnvironment
agentStayAndHarvestM e = agentStateFieldM sugAgCoord >>= (\coord -> agentHarvestCellM coord e)

agentMoveAndHarvestCellM :: Discrete2dCoord -> SugEnvironment -> State SugAgentOut SugEnvironment
agentMoveAndHarvestCellM cellCoord e = agentHarvestCellM cellCoord e >>= (\e' -> agentMoveToM cellCoord e')

agentMoveToM :: Discrete2dCoord -> SugEnvironment -> State SugAgentOut SugEnvironment
agentMoveToM cellCoord e = do
  e' <- unoccupyPositionM e

  updateAgentStateM (\s -> s { sugAgCoord = cellCoord })

  s <- agentStateM
  aid <- agentIdM

  let cell = cellAt cellCoord e'
  let cellOccupied = cell { sugEnvOccupier = Just (cellOccupier aid s) }
  return $ changeCellAt cellCoord cellOccupied e'

agentHarvestCellM :: Discrete2dCoord -> SugEnvironment -> State SugAgentOut SugEnvironment
agentHarvestCellM cellCoord e = do
  let cell = cellAt cellCoord e

  sugarLevelAgent <- agentStateFieldM sugAgSugarLevel
  spiceLevelAgent <- agentStateFieldM sugAgSpiceLevel

  let sugarLevelCell = sugEnvSugarLevel cell
  let spiceLevelCell = sugEnvSpiceLevel cell

  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent
  let newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

  updateAgentStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgSpiceLevel = newSpiceLevelAgent })

  let cellHarvested = cell { sugEnvSugarLevel = 0.0, sugEnvSpiceLevel = 0.0 }
  let e' = changeCellAt cellCoord cellHarvested e
  
  -- NOTE: at the moment harvesting SPICE does not influence the polution
  return $ poluteCell (sugarLevelCell * polutionHarvestFactor ) cellCoord e'

agentAgeingM :: Double -> SugEnvironment -> State SugAgentOut SugEnvironment
agentAgeingM newAge e = do
  updateAgentStateM (\s -> s { sugAgAge = newAge })

  ifThenElseM
      dieFromAgeM
      (do
          birthNewAgentM e
          passWealthOnM
          agentDiesM e)
      (return e)

birthNewAgentM :: SugEnvironment -> State SugAgentOut ()
birthNewAgentM e
    | not _enableBirthAgentOnAgeDeath_ = return ()
    | otherwise = do
      newAgentId <- agentIdM -- NOTE: in this case we keep the old id
      newAgentCoord <- findUnoccpiedRandomPositionM   -- NOTE: why not take the same position?
      newAgentDef <- agentRandomM $ randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviour sugarScapeAgentConversationM
      createAgentM newAgentDef
  where
    findUnoccpiedRandomPositionM :: State SugAgentOut Discrete2dCoord
    findUnoccpiedRandomPositionM = do
      (c, coord) <- agentRandomM $ randomCell e
      ifThenElse
          (cellOccupied c) 
          findUnoccpiedRandomPositionM 
          (return coord)

dieFromAgeM :: State SugAgentOut Bool
dieFromAgeM = do
  age <- agentStateFieldM sugAgAge
  maxAge <- agentStateFieldM sugAgMaxAge
  return $ age > maxAge

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSexM :: SugarScapeAgentIn -> SugEnvironment -> State SugAgentOut ()
agentSexM ain e 
  | not _enableSex_ = return ()
  | otherwise = whenM isFertileM (do
    coord <- agentStateFieldM sugAgCoord
    nids <- neighbourIdsM e
    let unncs = map fst (unoccupiedNeighbourhoodOfNeighbours coord e)
    agentMatingConversationM ain nids unncs)

agentMatingConversationM :: SugarScapeAgentIn 
                         -> [AgentId]
                         -> [Discrete2dCoord]
                         -> State SugAgentOut ()
agentMatingConversationM _ [] _ = conversationEndM
agentMatingConversationM _ _ [] = conversationEndM
agentMatingConversationM ain (receiverId:ais) allCs@(coord:cs) = do
    gender <- agentStateFieldM sugAgGender

    ifThenElseM
        satisfiesWealthForChildBearingM
        (conversationM 
            (receiverId, MatingRequest gender) 
            (conversationReplyMonadicRunner agentMatingConversationsReplyM))
        conversationEndM
    where
      agentMatingConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg) 
                                          -> SugEnvironment
                                          -> State SugAgentOut SugEnvironment
      agentMatingConversationsReplyM Nothing e = agentMatingConversationM ain ais allCs >> return e  -- NOTE: the target was not found or does not have a handler, continue with the next
      agentMatingConversationsReplyM (Just (_, MatingReplyNo)) e = agentMatingConversationM ain ais allCs >> return e
      agentMatingConversationsReplyM (Just (senderId, MatingReplyYes otherTup)) e = mate otherTup e
      agentMatingConversationsReplyM (Just _) e = agentMatingConversationM ain ais allCs >> return e  -- NOTE: unexpected/MatingChildAck reply, continue with the next

      mate :: MatingReplyTuple
              -> SugEnvironment 
              -> State SugAgentOut SugEnvironment
      mate otherTup e = do
        initialSugarEndow <- agentStateFieldM sugAgSugarInit

        let mySugarContribution = initialSugarEndow * 0.5
        mySugarMetab <- agentStateFieldM sugAgSugarMetab
        mySpiceMetab <- agentStateFieldM sugAgSpiceMetab
        myVision <- agentStateFieldM sugAgVision
        myCulturalTag <- agentStateFieldM sugAgCulturalTag
        myImmuneSysBorn <- agentStateFieldM sugAgImmuneSysBorn

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
        
        updateAgentStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s - mySugarContribution,
                                      sugAgChildren = newBornId : sugAgChildren s})
        
        createAgentM newBornDef

        conversationM 
            (receiverId, MatingChild newBornId)
            (conversationIgnoreReplyMonadicRunner $ agentMatingConversationM ain ais cs) -- NOTE: ignore the incoming environment because we just want to continue without being actually interested in the 

        return e'

handleMatingConversationM :: SugarScapeAgentGender
                          -> SugarScapeAgentState
                          -> (SugarScapeMsg, SugarScapeAgentState)
handleMatingConversationM otherGender s 
    | isFertile s &&
        satisfiesWealthForChildBearing s &&
        differentGender = (MatingReplyYes (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn), s')
    | otherwise = (MatingReplyNo, s)
  where
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

inheritSugarM :: SugarScapeAgentIn -> State SugAgentOut ()
inheritSugarM ain = onMessageMState inheritSugarActionM ain
  where
      inheritSugarActionM :: AgentMessage SugarScapeMsg -> State SugAgentOut ()
      inheritSugarActionM (_, InheritSugar sug) = updateAgentStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sug })
      inheritSugarActionM _ = return ()

agentCultureContactM :: SugarScapeAgentIn -> SugEnvironment -> State SugAgentOut ()
agentCultureContactM ain e = do
    onMessageMState cultureContactActionM ain

    nids <- neighbourIdsM e
    culturalTag <- agentStateFieldM sugAgCulturalTag

    broadcastMessageM (CulturalContact culturalTag) nids 
  where
    cultureContactActionM :: AgentMessage SugarScapeMsg -> State SugAgentOut ()
    cultureContactActionM (_, CulturalContact tagActive) = do
      agentTag <- agentStateFieldM sugAgCulturalTag
      agentTag' <- agentRandomM (cultureContact tagActive agentTag)
      
      let tribe = calculateTribe agentTag'

      updateAgentStateM (\s -> s { sugAgCulturalTag = agentTag', sugAgTribe = tribe})
    cultureContactActionM _ = return ()

agentKilledInCombatM :: SugarScapeAgentIn -> State SugAgentOut ()
agentKilledInCombatM ain = onMessageMState killedInCombatActionM ain
  where
    killedInCombatActionM :: AgentMessage SugarScapeMsg -> State SugAgentOut ()
    killedInCombatActionM (_, KilledInCombat) = killM -- NOTE: don't unoccupie position (as in agentdies) because it is occupied by the killer already
    killedInCombatActionM _ = return ()

agentMoveM :: SugEnvironment -> State SugAgentOut SugEnvironment
agentMoveM e
  | _enableCombat_ = agentCombatMoveM e
  | otherwise = agentNonCombatMoveM e

agentCombatMoveM :: SugEnvironment -> State SugAgentOut SugEnvironment
agentCombatMoveM e = do
    cellsInSight <- agentLookoutM e
    myTribe <- agentStateFieldM sugAgTribe
    myWealth <- agentStateFieldM sugAgSugarLevel 

    let targetCells = filterOccupiers (occupierCombatable myWealth myTribe) cellsInSight
    
    ifThenElse 
      (null targetCells)
      (agentNonCombatMoveM e)--(agentStayAndHarvestM e)
      (do
        coord <- agentStateFieldM sugAgCoord

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
        vulnerableToRetaliationM :: Double -> SugEnvironment -> State SugAgentOut Bool
        vulnerableToRetaliationM payoff e = do
          sugarLevelAgent <- agentStateFieldM sugAgSugarLevel
          cellsInSight <- agentLookoutM e
          myTribe <- agentStateFieldM sugAgTribe

          let futureSugarLevel = payoff + sugarLevelAgent
          let retaliatingCells = filterOccupiers (occupierRetaliator futureSugarLevel myTribe) cellsInSight

          return $ (not . null) retaliatingCells

        moveAndHarvestAndKillM :: ((Discrete2dCoord, SugEnvCell), Double) 
                               -> SugEnvironment 
                               -> State SugAgentOut SugEnvironment
        moveAndHarvestAndKillM ((cellCoord, cell), payoff) e = do
          sugarLevelAgent <- agentStateFieldM sugAgSugarLevel
          let newSugarLevelAgent = payoff + sugarLevelAgent

          e' <- unoccupyPositionM e
          updateAgentStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgCoord = cellCoord })

          aid <- agentIdM
          s <- agentStateM

          let cellHarvestedAndOccupied = cell {
            sugEnvSugarLevel = 0.0
          , sugEnvOccupier = Just (cellOccupier aid s)
          , sugEnvPolutionLevel = 0
          }

          let e'' = changeCellAt cellCoord cellHarvestedAndOccupied e'

          when 
              (cellOccupied cell) 
              (killOccupierOfCellM cell)

          return e''

        killOccupierOfCellM :: SugEnvCell -> State SugAgentOut ()
        killOccupierOfCellM cell = sendMessageM (occupierId, KilledInCombat)
          where
            occupier = fromJust $ sugEnvOccupier cell
            occupierId = sugEnvOccId occupier 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter IV: Sugar and Spice - Trade Comes to the Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentTradingM :: SugEnvironment -> State SugAgentOut ()
agentTradingM e 
    | _enableTrading_ = neighbourIdsM e >>= agentTradingConversationM
    | otherwise = return ()
  where
      agentTradingConversationM :: [AgentId]
                                -> State SugAgentOut ()
      agentTradingConversationM [] = conversationEndM
      agentTradingConversationM (receiverId:otherAis) = do
          s <- agentStateM
          let mrsSelf = agentMRS s
          conversationM 
              (receiverId, TradingOffer mrsSelf) 
              (conversationIgnoreEnvReplyMonadicRunner $ agentTradingConversationsReplyM mrsSelf)
        where
          agentTradingConversationsReplyM :: Double
                                          -> Maybe (AgentMessage SugarScapeMsg)
                                          -> State SugAgentOut ()
          agentTradingConversationsReplyM _ Nothing = agentTradingConversationM otherAis 
          agentTradingConversationsReplyM _ (Just (_, TradingRefuse)) = agentTradingConversationM otherAis 
          agentTradingConversationsReplyM _ (Just (_, TradingTransact _)) = agentTradingConversationM otherAis  -- NOTE: other agent has transacted, continue with next
          agentTradingConversationsReplyM mrsSelf (Just (senderId, TradingAccept mrsOther)) = do
            s <- agentStateM

            let welfareIncreases = agentTradeIncreaseWelfare s mrsOther
            
            ifThenElse 
              welfareIncreases (do
                    let s' = agentTradeExchange s mrsOther
                    setAgentStateM s'
                    conversationM 
                        (senderId, TradingTransact mrsSelf) 
                        (conversationIgnoreEnvReplyMonadicRunner $ agentTradingConversationsReplyM mrsSelf))
                (agentTradingConversationM otherAis)

agentCreditM :: SugarScapeAgentIn -> SugEnvironment -> State SugAgentOut ()
agentCreditM ain e 
  | _enableCredit_ = agentRequestCreditM e >> agentCheckCreditPaybackDueM >> agentCreditPaybackIncomingM ain >> agentCreditDeathIncomingM ain
  | otherwise = return ()

-- NOTE: for now only sugar is lended & borrowed, no spice
agentRequestCreditM :: SugEnvironment -> State SugAgentOut ()
agentRequestCreditM e = do
    nids <- neighbourIdsM e
    let hasNeighbours = not $ null nids
    when hasNeighbours (agentCreditConversationM nids)

  where
    agentCreditConversationM :: [AgentId]
                             -> State SugAgentOut ()
    agentCreditConversationM [] = conversationEndM
    agentCreditConversationM (receiverId:otherAis) = do
        s <- agentStateM

        ifThenElse 
            (isPotentialBorrower s)
            (conversationM (receiverId, CreditRequest) 
                (conversationIgnoreEnvReplyMonadicRunner agentCreditConversationsReplyM))
            conversationEndM
      where
          agentCreditConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg)
                                              -> State SugAgentOut ()
          agentCreditConversationsReplyM Nothing = agentCreditConversationM otherAis
          agentCreditConversationsReplyM (Just (_, CreditRequestRefuse)) = agentCreditConversationM otherAis 
          agentCreditConversationsReplyM (Just (lenderId, CreditOffer credit)) = do
            age <- agentStateFieldM sugAgAge

            let (faceValue, creditDuration, creditInterestRate) = credit
            let creditDueAge = age + creditDuration 
            let creditInfo = (lenderId, creditDueAge, credit)
            updateAgentStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + faceValue,
                                            sugAgBorrowingCredits = creditInfo : sugAgBorrowingCredits s })

            agentCreditConversationM otherAis

-- NOTE: if a borrower dies: notify the lenders so they know they take a loss (remove it from open credits)
-- NOTE: if a lender dies: notify the borrowers so they know they don't have to pay back
-- NOTE that we don't implement the inheritance-rule for loans
agentDeathHandleCreditsM :: State SugAgentOut ()
agentDeathHandleCreditsM = do
  borrowedCredits <- agentStateFieldM sugAgBorrowingCredits
  borrowerIds <- agentStateFieldM sugAgLendingCredits

  let lenderIds = map (\(lid, _, _) -> lid) borrowedCredits
  
  broadcastMessageM CreditBorrowerDied lenderIds
  broadcastMessageM CreditLenderDied borrowerIds

agentCreditDeathIncomingM :: SugarScapeAgentIn -> State SugAgentOut ()
agentCreditDeathIncomingM ain = onMessageMState creditDeathActionM ain
  where
    creditDeathActionM :: AgentMessage SugarScapeMsg -> State SugAgentOut ()
    creditDeathActionM (borrowerId, CreditBorrowerDied) = borrowerDiedM borrowerId
    creditDeathActionM (lenderId, CreditLenderDied) = lenderDiedM lenderId
    creditDeathActionM _ = return ()

    -- NOTE: the borrower could have borrowed multiple times from this lender, remove ALL ids
    borrowerDiedM :: AgentId -> State SugAgentOut ()
    borrowerDiedM borrowerId = do
      borrowers <- agentStateFieldM sugAgLendingCredits
      let borrowersRemoved = filter (/=borrowerId) borrowers
      updateAgentStateM (\s -> s { sugAgLendingCredits = borrowersRemoved } )

    -- NOTE: the lender could have lended multiple times to this borrower, remove ALL credits
    lenderDiedM :: AgentId -> State SugAgentOut ()
    lenderDiedM lenderId = do
      borrowedCredits <- agentStateFieldM sugAgBorrowingCredits
      let borrowersRemoved = filter (\(lId, _, _) -> lId /= lenderId) borrowedCredits
      updateAgentStateM (\s -> s { sugAgBorrowingCredits = borrowersRemoved } )

agentCreditPaybackIncomingM :: SugarScapeAgentIn -> State SugAgentOut ()
agentCreditPaybackIncomingM ain = onMessageMState creditPaybackActionM ain
  where
    creditPaybackActionM :: AgentMessage SugarScapeMsg -> State SugAgentOut ()
    creditPaybackActionM (_, (CreditPaybackHalf amount)) = halfCreditPaybackM amount
    creditPaybackActionM (borrowerId, (CreditPaybackFull amount)) = fullCreditPaybackM borrowerId amount
    creditPaybackActionM _ = return ()

    -- NOTE: in this case we don't remove the borrower because it has not yet payed back the whole credit
    halfCreditPaybackM :: Double -> State SugAgentOut ()
    halfCreditPaybackM = agentChangeSugarWealthM 

    -- NOTE: in this case we just remove the first borrower-id we find. It is possible that this lender has lended multiple times to the borrower but this doesnt matter in this case
    fullCreditPaybackM :: AgentId -> Double -> State SugAgentOut ()
    fullCreditPaybackM borrowerId amount = do
      borrowers <- agentStateFieldM sugAgLendingCredits
      let borrowersFirstRemoved = delete borrowerId borrowers
      updateAgentStateM (\s -> s { sugAgLendingCredits = borrowersFirstRemoved } )
      agentChangeSugarWealthM amount

agentChangeSugarWealthM :: Double -> State SugAgentOut ()
agentChangeSugarWealthM amount = updateAgentStateM (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + amount } )

agentCheckCreditPaybackDueM :: State SugAgentOut ()
agentCheckCreditPaybackDueM = do
    borrowedCredits <- agentStateFieldM sugAgBorrowingCredits

    borrowedCredits' <- foldM agentCheckCreditPaybackAuxM [] borrowedCredits
    updateAgentStateM (\s -> s { sugAgBorrowingCredits = borrowedCredits'})
  where
    agentCheckCreditPaybackAuxM :: [SugarScapeCreditInfo] 
                                -> SugarScapeCreditInfo 
                                -> State SugAgentOut [SugarScapeCreditInfo]
    agentCheckCreditPaybackAuxM accCredits creditInfo@(lenderId, ageDue, credit) = do
        age <- agentStateFieldM sugAgAge
        let creditDue = ageDue >= age

        ifThenElse creditDue
            (paybackCredit age)
            (return $ creditInfo : accCredits)
      where
          paybackCredit :: Double -> State SugAgentOut [SugarScapeCreditInfo]
          paybackCredit age = do
            wealth <- agentStateFieldM sugAgSugarLevel

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
                    -> SugarScapeAgentState
                    -> SugarScapeMsg
handleTradingOfferM mrsOther s 
    | welfareIncreases = TradingAccept mrsSelf     -- This makes the agent better off
    | otherwise = TradingRefuse                    -- This trade would make the agent worse off, refuse the trade
  where
    mrsSelf = agentMRS s
    welfareIncreases = agentTradeIncreaseWelfare s mrsOther

-- TODO: monadic-refactoring
handleTradingTransactM :: Double
                       -> SugarScapeAgentState
                       -> (SugarScapeMsg, SugarScapeAgentState)
handleTradingTransactM mrsOther s = (TradingTransact mrsOther, s') -- NOTE: simply reply with the same transaction-message
  where
    s' = agentTradeExchange s mrsOther

-- TODO: monadic-refactoring
handleCreditRequestM :: SugarScapeAgentState -> AgentId -> (SugarScapeMsg, SugarScapeAgentState)
handleCreditRequestM s borrowerId
    | isLender = (CreditOffer credit, s')
    | otherwise = (CreditRequestRefuse, s)
  where
    mayFaceValue = potentialLender s
    isLender = isJust mayFaceValue
    
    faceValue = fromJust mayFaceValue
    credit = (faceValue, lendingCreditDuration, lendingCreditInterestRate)

    s' = s { sugAgSugarLevel = sugAgSugarLevel s - faceValue,
            sugAgLendingCredits = borrowerId : sugAgLendingCredits s }
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter V: Disease Processes
------------------------------------------------------------------------------------------------------------------------
agentDiseaseContactM :: SugarScapeAgentIn -> State SugAgentOut ()
agentDiseaseContactM ain = onMessageMState diseaseContactActionM ain
  where
    diseaseContactActionM :: AgentMessage SugarScapeMsg -> State SugAgentOut ()
    diseaseContactActionM (_, DiseaseContact d) = updateAgentStateM (\s -> s { sugAgDiseases = d : (sugAgDiseases s) } )
    diseaseContactActionM _ = return ()

agentDiseasesTransmitM :: SugEnvironment -> State SugAgentOut ()
agentDiseasesTransmitM e = do
  diseases <- agentStateFieldM sugAgDiseases
  nids <- neighbourIdsM e

  let hasDiseases = (not . null) diseases
  let hasNeighbours = (not . null) nids

  when
      (hasDiseases && hasNeighbours)
      (do
          randDisease <- agentRandomPicksM diseases (length nids)
          let msgs = map (\(receiverId, disease) -> (receiverId, DiseaseContact disease)) (zip nids randDisease)
          sendMessagesM msgs)

agentImmunizeM :: State SugAgentOut ()
agentImmunizeM = do
  immuneSystem <- agentStateFieldM sugAgImmuneSys
  diseases <- agentStateFieldM sugAgDiseases

  let (immuneSystem', diseases') = foldr agentImmunizeAux (immuneSystem, []) diseases

  updateAgentStateM (\s -> s { sugAgImmuneSys = immuneSystem',
                                sugAgDiseases = diseases' })

agentDiseaseProcessesM :: SugarScapeAgentIn -> SugEnvironment -> State SugAgentOut ()
agentDiseaseProcessesM ain e = agentDiseaseContactM ain >> agentDiseasesTransmitM e >> agentImmunizeM
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CONVERSATION-HANDLER
------------------------------------------------------------------------------------------------------------------------
-- TODO: monadic-refactoring
sugarScapeAgentConversationM :: SugarScapeAgentConversation
sugarScapeAgentConversationM ain e (_, MatingRequest tup) = Just (s', m, e)
  where
    s = agentStateIn ain
    (m, s') = handleMatingConversationM tup s
sugarScapeAgentConversationM ain e (_, MatingChild childId) = Just (s', MatingChildAck, e)
  where
    s = agentStateIn ain
    s' = s { sugAgChildren = childId : sugAgChildren s}
sugarScapeAgentConversationM ain e (_, TradingOffer mrs) = Just (s, m, e)
  where
    s = agentStateIn ain
    m = handleTradingOfferM mrs s
sugarScapeAgentConversationM ain e (_, TradingTransact mrs) = Just (s', m, e)
  where
    s = agentStateIn ain
    (m, s') = handleTradingTransactM mrs s
sugarScapeAgentConversationM ain e (borrowerId, CreditRequest) = Just (s', m, e)
  where
    s = agentStateIn ain
    (m, s') = handleCreditRequestM s borrowerId
sugarScapeAgentConversationM _ _ _ = Nothing
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- BEHAVIOUR-CONTROL
-- NOTE: although we could configure each chapter separately we also provide separate chapter-functions which never call given functions
------------------------------------------------------------------------------------------------------------------------
chapterII :: SugEnvironment 
          -> Double 
          -> SugarScapeAgentIn 
          -> State SugAgentOut SugEnvironment
chapterII e age ain = do     
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

chapterIII :: SugEnvironment 
           -> Double 
           -> SugarScapeAgentIn 
           -> State SugAgentOut SugEnvironment
chapterIII e age ain = do
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

chapterIV :: SugEnvironment 
          -> Double 
          -> SugarScapeAgentIn 
          -> State SugAgentOut SugEnvironment
chapterIV e age ain = do     
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

chapterV :: SugEnvironment 
         -> Double 
         -> SugarScapeAgentIn 
         -> State SugAgentOut SugEnvironment
chapterV e age ain = do     
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
-}