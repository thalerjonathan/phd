{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Agent.Mating
  ( prop_agent_acceptMatingRequest
  , prop_agent_handleMatingRequest
  ) where

import Control.Monad.State.Strict
import Test.Tasty.QuickCheck as QC

import Agent.Agent
import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Mating 
import SugarScape.Core.Model
import SugarScape.Core.Scenario

-- NOTE: problem is that it contains AgentDef which in turn contains MSFs, which 
-- can not be checked for equality (at least not in Haskell), ignoring MSF
instance Eq (SugAgentOut g) where
  (==) ao0 ao1 = aoKill ao0       == aoKill ao1 &&
                 aoCreate ao0     == aoCreate ao1 &&
                 aoObservable ao0 == aoObservable ao1 &&
                 aoEvents ao0     == aoEvents ao1

-- NOTE: AgentDef contains MSF, which 
-- can not be checked for equality (at least not in Haskell), ignoring MSF
instance Eq (SugAgentDef g) where
  (==) ad0 ad1 = adId ad0      == adId ad1 && 
                 adInitObs ad0 == adInitObs ad1 

instance Arbitrary AgentGender where
  -- arbitrary :: Gen AgentGender
  arbitrary = elements [Male, Female]

-- NOTE: no need to test isFertile, is done implicitly with prop_agent_acceptMatingRequest 

-- NOTE: need quite some customisation of agent state => run in Gen
prop_agent_acceptMatingRequest :: AgentGender -> Gen Bool
prop_agent_acceptMatingRequest otherGender = do
    as0 <- arbitraryAgentStateFromScenario mkParamsAnimationIII_1

    -- NOTE: need adjust sugar level, spice level, and age, otherwise always false
    randSugLvl <- choose (0, 150)
    randSpiLvl <- choose (0, 150)
    randAge    <- choose (0, 100)
    let as = as0 { sugAgSugarLevel = randSugLvl
                 , sugAgSpiceLevel = randSpiLvl
                 , sugAgAge        = randAge }

    return $ prop_agent_acceptMatingRequest_prop as
  where
    prop_agent_acceptMatingRequest_prop :: SugAgentState -> Bool
    prop_agent_acceptMatingRequest_prop as 
        = accept == (genderDiffer && agentFertile) 
            && agentStateUnchanged
      where
        (accept, as')    = runState (acceptMatingRequest otherGender) as

        genderDiffer     = otherGender /= sugAgGender as

        agentFertile     = withinFertileAge && hasFertileWealth
        withinFertileAge = sugAgAge as >= fst (sugAgFertAgeRange as)
                            && sugAgAge as <= snd (sugAgFertAgeRange as)
        hasFertileWealth = sugAgSugarLevel as >= sugAgInitSugEndow as 
                            && sugAgSpiceLevel as >= sugAgInitSpiEndow as
        -- agent-state must not change during this function call
        agentStateUnchanged = as == as'

prop_agent_handleMatingRequest :: AgentGender -> Gen Bool
prop_agent_handleMatingRequest otherGender = do
    as0 <- arbitraryAgentStateFromScenario mkParamsAnimationIII_1

    -- NOTE: need adjust sugar level, spice level, and age, otherwise never fertile
    randSugLvl <- choose (0, 150)
    randSpiLvl <- choose (0, 150)
    randAge    <- choose (0, 100)
    let as = as0 { sugAgSugarLevel = randSugLvl
                 , sugAgSpiceLevel = randSpiLvl
                 , sugAgAge        = randAge }

    return $ prop_agent_handleMatingRequest_prop as
  where
    prop_agent_handleMatingRequest_prop :: SugAgentState -> Bool
    prop_agent_handleMatingRequest_prop as 
        | accept    = ao == aoExpAccept
        | otherwise = ao == aoExpRefuse
      where
        -- need to know if the agent has accepted the request, output depends on
        -- it - acceptMatingRequest is tested in a different property test.
        -- Does NOT change the agent-state, can ignore it using evalState
        accept = evalState (acceptMatingRequest otherGender) as

        senderId  = 42
        (ao, as') = runState (handleMatingRequest senderId otherGender) as

        expAccept   = (sugAgSugarLevel as / 2, 
                       sugAgSpiceLevel as / 2, 
                       sugAgSugarMetab as, 
                       sugAgVision as, 
                       sugAgCultureTag as,
                       sugAgImSysGeno as)
        aoExpAccept = sendEventTo senderId (MatingReply $ Just expAccept) (agentObservable as)
        aoExpRefuse = sendEventTo senderId (MatingReply Nothing) (agentObservable as) 

-- TODO: handleMatingTx