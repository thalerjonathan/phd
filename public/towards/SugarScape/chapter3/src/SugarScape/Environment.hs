module SugarScape.Environment 
  ( SugEnvBehaviour
  , sugEnvBehaviour
  ) where

import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model

-- NOTE: the environment behaviour is a pure comuptation, 
-- because there is no need for any monadic behaviour 
-- or access of absstate 
type SugEnvBehaviour = Time -> SugEnvironment -> SugEnvironment

sugEnvBehaviour :: SugarScapeParams 
                -> Time
                -> SugEnvironment
                -> SugEnvironment
sugEnvBehaviour params t env = env''
  where
    env'  = regrowSugar (spSugarRegrow params) t env
    env'' = polutionDiffusion (spPolutionDiffusion params) t env'

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
    fs = map (\(coord, c) -> do
          let ncs  = neighbourCells coord True env
          let flux = sum (map sugEnvSitePolutionLevel ncs) / fromIntegral (length ncs)
          flux) cs

    env' = foldr (\((coord, c), flux) acc -> do
            let c' = c { sugEnvSitePolutionLevel = flux }
            changeCellAt coord c' acc) env (zip cs fs)

regrowSugar :: SugarRegrow 
            -> Time
            -> SugEnvironment
            -> SugEnvironment
regrowSugar Immediate   _ = regrowSugarToMax
regrowSugar (Rate rate) _ = regrowSugarByRate rate
regrowSugar (Season summerRate winterRate seasonDuration) t
                          = regrowSugarBySeason t summerRate winterRate seasonDuration

regrowSugarToMax :: SugEnvironment -> SugEnvironment
regrowSugarToMax = updateCells (\c -> c { sugEnvSiteSugarLevel = sugEnvSiteSugarCapacity c})

regrowSugarByRate :: Double -> SugEnvironment -> SugEnvironment
regrowSugarByRate rate = updateCells $ regrowSugarInSiteWithRate rate

regrowSugarBySeason :: Time
                    -> Double
                    -> Double
                    -> Time
                    -> SugEnvironment
                    -> SugEnvironment
regrowSugarBySeason t summerRate winterRate seasonDuration 
    = updateCellsWithCoords (\((_, y), c) -> 
        if y <= half
          then regrowSugarInSiteWithRate topate c
          else regrowSugarInSiteWithRate bottomRate c)
  where
    half       = floor (fromIntegral (snd sugarscapeDimensions) / 2 :: Double)

    isSummer   = even (floor ((fromIntegral t / fromIntegral seasonDuration) :: Double) :: Integer)
    topate     = if isSummer then summerRate     else 1 / winterRate
    bottomRate = if isSummer then 1 / winterRate else summerRate

regrowSugarInSiteWithRate :: Double 
                          -> SugEnvSite
                          -> SugEnvSite
regrowSugarInSiteWithRate rate c 
  = c { sugEnvSiteSugarLevel = 
          min
              (sugEnvSiteSugarCapacity c)
              ((sugEnvSiteSugarLevel c) + rate)} -- if this bracket is omited it leads to a bug: all environment cells have +1 level