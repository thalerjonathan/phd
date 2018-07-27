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

import Control.Concurrent.STM
import Control.Monad.Random
import Control.Monad.Reader
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
                  ((sugEnvSugarLevel c) + rate)})

    regrowSugarToMax :: (MonadState SugEnvironment m) => m ()
    regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

regrow :: (MonadState SugEnvironment m) => m ()
regrow = regrowSugar sugarGrowbackUnits

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  ctx <- arrM_ (lift ask) -< ()
  let envVar = sugCtxEnv ctx
  env <- arrM (lift . lift . lift . readTVar) -< envVar

  (_, env') <- arrM (lift . runStateT regrow) -< env

  arrM (\(envVar, env') -> lift $ lift $ lift $ writeTVar envVar env') -< (envVar, env')

  returnA -< agentOut