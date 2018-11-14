module SugarScape.Core.Environment 
  ( SugEnvBehaviour
  , sugEnvBehaviour
  ) where

import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model

type RegrowToMaxFunc = SugEnvSite -> SugEnvSite
type RegrowByRateFunc = Double -> SugEnvSite -> SugEnvSite

-- NOTE: the environment behaviour is a pure comuptation, 
-- because there is no need for any monadic behaviour 
-- or access of absstate 
type SugEnvBehaviour = Time -> SugEnvironment -> SugEnvironment

sugEnvBehaviour :: SugarScapeScenario 
                -> Time
                -> SugEnvironment
                -> SugEnvironment
sugEnvBehaviour params t env = env'''
  where
    env'   = regrow (spSugarRegrow params) regrowSugarToMax regrowSugarWithRate t env
    env''  = regrow (spSpiceRegrow params) regrowSpiceToMax regrowSpiceWithRate t env'
    env''' = polutionDiffusion (spPolutionDiffusion params) t env''

polutionDiffusion :: Maybe Int
                  -> Time
                  -> SugEnvironment
                  -> SugEnvironment
polutionDiffusion Nothing _ env = env
polutionDiffusion (Just d) t env
    | not timeForDiffusion = env
    | otherwise = env'
  where
    timeForDiffusion = 0 == mod t d

    cs = allCellsWithCoords env
    fs = map (\(coord, _) -> do
          let ncs  = neighbourCells coord True env
          let flux = sum (map sugEnvSitePolutionLevel ncs) / fromIntegral (length ncs)
          flux) cs

    env' = foldr (\((coord, c), flux) acc -> do
            let c' = c { sugEnvSitePolutionLevel = flux }
            changeCellAt coord c' acc) env (zip cs fs)


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
       -> Time
       -> SugEnvironment
       -> SugEnvironment
regrow Immediate maxFun _ _    = updateCells maxFun
regrow (Rate rate) _ rateFun _ = updateCells $ rateFun rate
regrow (Season summerRate winterRate seasonDuration) _ rateFun t
  = regrowBySeason rateFun t summerRate winterRate seasonDuration

regrowBySeason :: RegrowByRateFunc
               -> Time
               -> Double
               -> Double
               -> Time
               -> SugEnvironment
               -> SugEnvironment
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
