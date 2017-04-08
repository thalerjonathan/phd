-----------------------------------------------------------------------------------------
-- |
-- Module      :  Segregation.Segregation
-- Copyright   :  (c) Jonathan Thaler, University of Nottingham, 2017
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  thaler.jonathan@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
--
-- Implementation of the Schelling Segregation with recursive simulation features (MetaABS)
--
--
-- Main Segregation modules:
--
-- * "Segregation.Segregation"            -- This exports all Segregation-related functions
--
-- * "Segregation.SegregationInit"        -- For initializing a Segregation-Simulation
--
-- * "Segregation.SegregationModel"       -- The Segregation implementation
--
-- * "Segregation.SegregationRun"           -- for running segregation
--
-- * "Segregation.SegregationStats"           -- calculating stats for the segregation-simulation
--
-----------------------------------------------------------------------------------------
module Segregation.Segregation where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import Segregation.SegregationModel
import Segregation.SegregationInit