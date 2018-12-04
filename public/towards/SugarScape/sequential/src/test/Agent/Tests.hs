{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- disable warning for unused imports, need to import Arbitrary instances
module Agent.Tests
  ( agentTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Agent.Agent  -- need this import for Arbitrary instance for AgentState
import Agent.Ageing
import Agent.Mating
import Agent.Metabolism
import Agent.Move
import Environment.Environment -- need this import for Arbitrary instance for SugEnvSite

agentTests :: TestTree 
agentTests = testGroup "Agent Tests"
              [ test_sselectBestSites_group
              , QC.testProperty "Ageing by DTime" prop_agent_ageing
              , QC.testProperty "Die Of Age" prop_agent_dieOfAge
              , QC.testProperty "Starved To Death Sugar only" prop_agent_starved_sugaronly
              , QC.testProperty "Starved To Death Sugar and Spice" prop_agent_starved_sugarandspice
              , QC.testProperty "Metabolism Sugar only" prop_agent_metabolism_sugaronly
              , QC.testProperty "Accept Mating Request" prop_agent_acceptMatingRequest ]

test_sselectBestSites_group :: TestTree
test_sselectBestSites_group = 
    testGroup "selectBestSites bestSugarLevel" 
        [test_selectBestSites_bestSugarLevel_empty,
         test_selectBestSites_bestSugarLevel_singlebestsamedist,
         test_selectBestSites_bestSugarLevel_multibestsamedist,
         test_selectBestSites_bestSugarLevel_singlebestdiffdist,
         test_selectBestSites_bestSugarLevel_allbestsamedist]