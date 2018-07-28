{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
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

regrowSugar :: (MonadState SugEnvironment m)
            => Double 
            -> m ()
regrowSugar rate
    | rate < 0 = regrowSugarToMax
    | otherwise = regrowSugarByRate
  where
    regrowSugarByRate :: (MonadState SugEnvironment m)
                      => m ()
    regrowSugarByRate  
      = updateCellsM (\c -> 
        c { sugEnvSugarLevel = 
              min
                  (sugEnvSugarCapacity c)
                  ((sugEnvSugarLevel c) + rate)}) -- if this bracket is omited it leads to a bug: all environment cells have +1 level

    regrowSugarToMax :: (MonadState SugEnvironment m) => m ()
    regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

regrow :: (MonadState SugEnvironment m) => m ()
regrow = regrowSugar sugarGrowbackUnits

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  env       <- arrM_ (lift readEnvironment)    -< ()
  (_, env') <- arrM  (lift . runStateT regrow) -< env
  _         <- arrM  (lift . writeEnvironment) -< env'

  returnA -< agentOut