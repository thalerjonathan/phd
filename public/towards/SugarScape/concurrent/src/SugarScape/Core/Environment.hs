module SugarScape.Core.Environment 
  ( SugEnvBehaviour
  , sugEnvBehaviour
  ) where

import Control.Monad
import Control.Monad.STM

import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario

type RegrowToMaxFunc = SugEnvSite -> SugEnvSite
type RegrowByRateFunc = Double -> SugEnvSite -> SugEnvSite

-- NOTE: environment is a STM computation because environment is stored in TArray
type SugEnvBehaviour = Time -> SugEnvironment -> STM ()

sugEnvBehaviour :: SugarScapeScenario -> SugEnvBehaviour
sugEnvBehaviour params t env = do
  regrow (spSugarRegrow params) regrowSugarToMax regrowSugarWithRate t env
  regrow (spSpiceRegrow params) regrowSpiceToMax regrowSpiceWithRate t env
  polutionDiffusion (spPolutionDiffusion params) t env

polutionDiffusion :: Maybe Int -> SugEnvBehaviour
polutionDiffusion Nothing _ _ = return ()
polutionDiffusion (Just d) t env
    | not timeForDiffusion = return ()
    | otherwise = do
      cs <- allCellsWithCoords env
      fs <- mapM (\(coord, _) -> do
              ncs <- neighbourCells coord True env
              let flux = sum (map sugEnvSitePolutionLevel ncs) / fromIntegral (length ncs)
              return flux) cs

      zipWithM_ (\(coord, c) flux -> do
                let c' = c { sugEnvSitePolutionLevel = flux }
                changeCellAt coord c' env) cs fs
  where
    timeForDiffusion = 0 == mod t d


regrowSugarWithRate :: RegrowByRateFunc
regrowSugarWithRate rate c 
  = c { sugEnvSiteSugarLevel = 
          min
              (sugEnvSiteSugarCapacity c)
              ((sugEnvSiteSugarLevel c) + rate)} -- if this bracket is omited it leads to a bug: all environment cells have +1 level

regrowSugarToMax :: RegrowToMaxFunc
regrowSugarToMax c = c { sugEnvSiteSugarLevel = sugEnvSiteSugarCapacity c}

regrowSpiceWithRate :: RegrowByRateFunc
regrowSpiceWithRate rate c 
  = c { sugEnvSiteSpiceLevel = 
          min
              (sugEnvSiteSpiceCapacity c)
              ((sugEnvSiteSpiceLevel c) + rate)} -- if this bracket is omited it leads to a bug: all environment cells have +1 level

regrowSpiceToMax :: RegrowToMaxFunc
regrowSpiceToMax c = c { sugEnvSiteSpiceLevel = sugEnvSiteSpiceCapacity c}

regrow :: Regrow 
       -> RegrowToMaxFunc
       -> RegrowByRateFunc
       -> SugEnvBehaviour
regrow Immediate maxFun _ _    = updateCells maxFun
regrow (Rate rate) _ rateFun _ = updateCells $ rateFun rate
regrow (Season summerRate winterRate seasonDuration) _ rateFun t
  = regrowBySeason rateFun t summerRate winterRate seasonDuration

regrowBySeason :: RegrowByRateFunc
               -> Time
               -> Double
               -> Double
               -> SugEnvBehaviour
regrowBySeason rateFun t summerRate winterRate seasonDuration 
    = updateCellsWithCoords (\((_, y), c) -> 
        if y <= half
          then rateFun topate c
          else rateFun bottomRate c)
  where
    half       = floor (fromIntegral (snd sugarscapeDimensions) / 2 :: Double)

    isSummer   = even (floor ((fromIntegral t / fromIntegral seasonDuration) :: Double) :: Integer)
    topate     = if isSummer then summerRate     else 1 / winterRate
    bottomRate = if isSummer then 1 / winterRate else summerRate
