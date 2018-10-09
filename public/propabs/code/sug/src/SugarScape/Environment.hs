{-# LANGUAGE Arrows #-}
module SugarScape.Environment 
  ( cellUnoccupied
  , cellOccupied
  
  , sugEnvironment
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Model

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
cellOccupied :: SugEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupier cell

cellUnoccupied :: SugEnvCell -> Bool
cellUnoccupied = not . cellOccupied

regrowSugar :: RandomGen g
            => Double 
            -> StateT SugEnvironment (Rand g) ()
regrowSugar rate
    | rate < 0  = regrowSugarToMax
    | otherwise = regrowSugarByRate
  where
    regrowSugarByRate :: RandomGen g
                      => StateT SugEnvironment (Rand g) ()
    regrowSugarByRate  
      = updateCellsM (\c -> 
        c { sugEnvSugarLevel = 
              min
                  (sugEnvSugarCapacity c)
                  ((sugEnvSugarLevel c) + rate)}) -- if this bracket is omited it leads to a bug: all environment cells have +1 level

    regrowSugarToMax :: StateT SugEnvironment (Rand g) ()
    regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

-- TODO: get rid of Rand g
sugEnvironment :: RandomGen g 
               => Double
               -> SugAgent g
sugEnvironment rate = proc _ -> do
  arrM_ (lift $ lift $ regrowSugar rate) -< ()
  returnA -< agentOut