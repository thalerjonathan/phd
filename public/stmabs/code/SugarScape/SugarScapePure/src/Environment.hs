{-# LANGUAGE Arrows #-}
module Environment 
  (
    cellUnoccupied
  , cellOccupied
  
  , regrow
  , sugEnvironment
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

import AgentMonad
import Discrete
import Model

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
                  (sugEnvSugarLevel c) + rate})

    regrowSugarToMax :: StateT SugEnvironment (Rand g) ()
    regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

regrow :: RandomGen g 
       => StateT SugEnvironment (Rand g) ()
regrow = regrowSugar sugarGrowbackUnits

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  arrM_ (lift $ lift  regrow) -< ()
  returnA -< agentOut