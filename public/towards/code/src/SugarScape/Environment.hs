{-# LANGUAGE Arrows #-}
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

polutionDiffusion :: Maybe Int
                  -> Time
                  -> StateT SugEnvironment (Rand g) ()
polutionDiffusion Nothing _  = return ()
polutionDiffusion (Just d) t 
    | not doDiffusion = return ()
    | otherwise = do
      cs <- allCellsWithCoordsM
      fs <- mapM (\(coord, c) -> do
        ncs <- neighbourCellsM coord True
        let flux = sum (map sugEnvCellPolutionLevel ncs) / fromIntegral (length ncs)
        return flux) cs

      zipWithM_ (\(coord, c) flux -> do
        let c' = c { sugEnvCellPolutionLevel = flux }
        changeCellAtM coord c') cs fs
  where
    doDiffusion = 0 == mod (floor t) d

regrowSugar :: RandomGen g
            => SugarRegrow 
            -> Time
            -> StateT SugEnvironment (Rand g) ()
regrowSugar Immediate   _ = regrowSugarToMax
regrowSugar (Rate rate) _ = regrowSugarByRate rate
regrowSugar (Season summerRate winterRate seasonDuration) t
  = regrowSugarBySeason t summerRate winterRate seasonDuration

regrowSugarToMax :: StateT SugEnvironment (Rand g) ()
regrowSugarToMax = updateCellsM (\c -> c { sugEnvCellSugarLevel = sugEnvCellSugarCapacity c})

regrowSugarByRate :: RandomGen g
                  => Double
                  -> StateT SugEnvironment (Rand g) ()
regrowSugarByRate rate = updateCellsM $ regrowSugarInCellWithRate rate

regrowSugarBySeason :: RandomGen g
                    => Time
                    -> Double
                    -> Double
                    -> Int
                    -> StateT SugEnvironment (Rand g) ()
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

regrowSugarInCellWithRate :: Double -> SugEnvCell -> SugEnvCell 
regrowSugarInCellWithRate rate c 
  = c { sugEnvCellSugarLevel = 
          min
              (sugEnvCellSugarCapacity c)
              ((sugEnvCellSugarLevel c) + rate)} -- if this bracket is omited it leads to a bug: all environment cells have +1 level

-- TODO: can we get rid of Rand g ?
sugEnvironment :: RandomGen g 
               => SugarScapeParams
               -> SugAgent g
sugEnvironment params = proc _ -> do
  t <- time -< ()
  arrM (lift . lift . envBehaviour params) -< t
  returnA -< agentOut