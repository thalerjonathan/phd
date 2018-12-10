{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Environment 
  ( sugEnvironment
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Model

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
envBehaviour :: RandomGen g
             => SugarScapeParams 
             -> Time
             -> StateT SugEnvironment (Rand g) ()
envBehaviour params t = do
  regrowSugar (spSugarRegrow params) t
  polutionDiffusion (spPolutionDiffusion params) t

polutionDiffusion :: MonadState SugEnvironment m
                  => Maybe Int
                  -> Time
                  -> m ()
polutionDiffusion Nothing _  = return ()
polutionDiffusion (Just d) t 
    | not doDiffusion = return ()
    | otherwise = do
      cs <- allCellsWithCoordsM
      fs <- mapM (\(coord, _) -> do
        ncs <- neighbourCellsM coord True
        let flux = sum (map sugEnvCellPolutionLevel ncs) / fromIntegral (length ncs)
        return flux) cs

      zipWithM_ (\(coord, c) flux -> do
        let c' = c { sugEnvCellPolutionLevel = flux }
        changeCellAtM coord c') cs fs
  where
    doDiffusion = 0 == mod (floor t) d

regrowSugar :: MonadState SugEnvironment m
            => SugarRegrow 
            -> Time
            -> m ()
regrowSugar Immediate   _ = regrowSugarToMax
regrowSugar (Rate rate) _ = regrowSugarByRate rate
regrowSugar (Season summerRate winterRate seasonDuration) t
                          = regrowSugarBySeason t summerRate winterRate seasonDuration

regrowSugarToMax :: MonadState SugEnvironment m => m ()
regrowSugarToMax = updateCellsM (\c -> c { sugEnvCellSugarLevel = sugEnvCellSugarCapacity c})

regrowSugarByRate :: MonadState SugEnvironment m
                  => Double
                  -> m ()
regrowSugarByRate rate = updateCellsM $ regrowSugarInCellWithRate rate

regrowSugarBySeason :: MonadState SugEnvironment m
                    => Time
                    -> Double
                    -> Double
                    -> Int
                    -> m ()
regrowSugarBySeason t summerRate winterRate seasonDuration 
    = updateCellsWithCoordsM (\((_, y), c) -> 
        if y <= half
          then regrowSugarInCellWithRate topate c
          else regrowSugarInCellWithRate bottomRate c)
  where
    half       = floor (fromIntegral (snd sugarscapeDimensions) / 2 :: Double)

    isSummer   = even (floor ((t / fromIntegral seasonDuration) :: Double) :: Integer)
    topate     = if isSummer then summerRate     else 1 / winterRate
    bottomRate = if isSummer then 1 / winterRate else summerRate

regrowSugarInCellWithRate :: Double 
                          -> SugEnvCell 
                          -> SugEnvCell 
regrowSugarInCellWithRate rate c 
  = c { sugEnvCellSugarLevel = 
          min
              (sugEnvCellSugarCapacity c)
              ((sugEnvCellSugarLevel c) + rate)} -- if this bracket is omited it leads to a bug: all environment cells have +1 level

sugEnvironment :: RandomGen g 
               => SugarScapeParams
               -> SugAgent g
sugEnvironment params = proc _ -> do
  t <- time -< ()
  arrM (lift . lift . envBehaviour params) -< t
  returnA -< agentOut