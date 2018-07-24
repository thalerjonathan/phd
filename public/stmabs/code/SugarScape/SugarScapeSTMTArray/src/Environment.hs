{-# LANGUAGE Arrows           #-}
module Environment 
  (
    cellUnoccupied
  , cellOccupied
  
  , regrow

  , sugEnvironment
  ) where

import Data.Maybe

import Control.Concurrent.STM
import Control.Monad.Random
import FRP.BearRiver

import Common
import Discrete
import Model

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
cellOccupied :: SugEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupier cell

cellUnoccupied :: SugEnvCell -> Bool
cellUnoccupied = not . cellOccupied

regrowSugar :: Double 
            -> SugEnvironment
            -> STM ()
regrowSugar rate env
    | rate < 0  = regrowSugarToMax
    | otherwise = regrowSugarByRate
  where
    regrowSugarByRate :: STM ()
    regrowSugarByRate  
      = updateAllCells (\c -> 
        c { sugEnvSugarLevel = 
              min
                  (sugEnvSugarCapacity c)
                  (sugEnvSugarLevel c) + rate}) env

    regrowSugarToMax :: STM ()
    regrowSugarToMax = updateAllCells (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c}) env

regrow :: SugEnvironment -> STM ()
regrow env = regrowSugar sugarGrowbackUnits env

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  env <- arrM_ (lift getEnvironment) -< ()

  arrM (lift . lift . lift . regrow) -< env

  returnA -< agentOut