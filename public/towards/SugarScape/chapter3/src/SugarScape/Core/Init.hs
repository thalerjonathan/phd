module SugarScape.Core.Init 
  ( createSugarScape
  ) where

import Control.Monad.Random

import Data.Char
import Data.List

import SugarScape.Agent.Agent
import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Random

createSugarScape :: RandomGen g
                 => SugarScapeScenario
                 -> Rand g ([(AgentId, SugAgentObservable, SugAgentMSF g)], SugEnvironment)
createSugarScape params = do
  ras <- agentDistribution params (sgAgentDistribution params)

  let as             = map (\(ad, _) -> (adId ad, adInitObs ad, adSf ad)) ras
      occupations    = map (\(ad, s) -> (sugAgCoord s, (adId ad, s))) ras

      sugarSpecs     = envSpec
      spiceSpecs     = map reverse envSpec

      sugSpiceSpecs  = parseEnvSpec sugarSpecs spiceSpecs
      sugSpiceCoords = specToCoords sugSpiceSpecs sugarscapeDimensions

      sites          = createSites params sugSpiceCoords occupations
      env            = createDiscrete2d
                        sugarscapeDimensions
                        neumann
                        WrapBoth
                        sites

  return (as, env)

agentDistribution :: RandomGen g
                  => SugarScapeScenario
                  -> AgentDistribution
                  -> Rand g [(SugAgentDef g, SugAgentState)]
agentDistribution params CombatCorners = do
  let agentCount  = sgAgentCount params
      halfCount   = floor ((fromIntegral agentCount / 2) :: Double)
      aisTopRight = [1 .. halfCount]
      aisBotLeft  = [halfCount + 1 .. agentCount]

  randCoordsTopRight <- randomCoords (30, 30) sugarscapeDimensions (length aisTopRight)
  rasTopRight        <- mapM (\(aid, coord) -> randomAgent params (aid, coord) agentMsf (changeToRedTribe params)) (zip aisTopRight randCoordsTopRight)

  randCoordsBotLeft <- randomCoords (0,0) (20, 20) (length aisBotLeft)
  rasBotLeft        <- mapM (\(aid, coord) -> randomAgent params (aid, coord) agentMsf (changeToBlueTribe params)) (zip aisBotLeft randCoordsBotLeft)

  return (rasBotLeft ++ rasTopRight)

agentDistribution params dist = do
  let agentCount = sgAgentCount params
      ais        = [1..agentCount]
      coordDims  = case dist of 
                    Scatter      -> sugarscapeDimensions
                    (Corner dim) -> dim
                    _            -> error "missing AgentDistribution case, programming fault, shouldn't happen!"
  
  randCoords <- randomCoords (0,0) coordDims agentCount
  mapM (\(aid, coord) -> randomAgent params (aid, coord) agentMsf id) (zip ais randCoords)

specToCoords :: [[(Int, Int)]]
             -> Discrete2dCoord
             -> [(Discrete2dCoord, Int, Int)]
specToCoords specs (dimX, dimY) 
    | length specs /= dimY = error ("sugar row count does not match y-dimensions: " ++ show (length specs) ++ " not " ++ show dimY)
    | otherwise            = specLines specs 0 []
  where
    specLines :: [[(Int, Int)]]
              -> Int
              -> [(Discrete2dCoord, Int, Int)]
              -> [(Discrete2dCoord, Int, Int)]
    specLines [] _ acc       = acc
    specLines (l : ls) y acc 
        | length l /= dimX = error ("sugar spec line size does not match x-dimensions: " ++ show (length l) ++ " not " ++ show dimX)
        | otherwise        = specLines ls (y + 1) acc'
      where
        acc' = specLineToCoords l 0 acc

        specLineToCoords :: [(Int, Int)]
                         -> Int
                         -> [(Discrete2dCoord, Int, Int)]
                         -> [(Discrete2dCoord, Int, Int)]
        specLineToCoords [] _ accLine              = accLine
        specLineToCoords ((su, sp) : ss) x accLine = specLineToCoords ss (x + 1) accLine'
          where
            accLine' = ((x, y), su, sp) : accLine

parseEnvSpec :: [String]
               -> [String]
               -> [[(Int, Int)]]
parseEnvSpec = zipWith parseEnvSpecLine
  where
    parseEnvSpecLine :: String 
                       -> String
                       -> [(Int, Int)]
    parseEnvSpecLine sugLine spiceLine 
        = reverse $ zipWith parseEnvSpecAux sugLine spiceLine
      where
        parseEnvSpecAux :: Char
                          -> Char
                          -> (Int, Int)
        parseEnvSpecAux su sp 
          | isNumber su && isNumber sp = (digitToInt su, digitToInt sp)
          | otherwise  = error "bad character in environment specification"

createSites :: SugarScapeScenario
            -> [(Discrete2dCoord, Int, Int)]
            -> [(Discrete2dCoord, (AgentId, SugAgentState))]
            -> [(Discrete2dCoord, SugEnvSite)]
createSites params siteSpecs occupations 
  = map (initRandomSite params occupations) siteSpecs
 
initRandomSite :: SugarScapeScenario
               -> [(Discrete2dCoord, (AgentId, SugAgentState))] 
               -> (Discrete2dCoord, Int, Int) 
               -> (Discrete2dCoord, SugEnvSite)
initRandomSite params os (coord, sugar, spice) 
    = (coord, c)
  where
    mayOccupier = Data.List.find ((==coord) . fst) os
    occ         = maybe Nothing (\(_, (aid, s)) -> (Just $ occupier aid s)) mayOccupier

    spice'      = if spSpiceEnabled params then spice else 0

    c = SugEnvSite {
      sugEnvSiteSugarCapacity = fromIntegral sugar
    , sugEnvSiteSugarLevel    = fromIntegral sugar
    
    , sugEnvSiteSpiceCapacity = fromIntegral spice'
    , sugEnvSiteSpiceLevel    = fromIntegral spice'
    
    , sugEnvSiteOccupier      = occ
    , sugEnvSitePolutionLevel = 0
    }

randomCoords :: RandomGen g
             => Discrete2dDimension 
             -> Discrete2dDimension 
             -> Int 
             -> Rand g [Discrete2dCoord]
randomCoords (minX, minY) (maxX, maxY) n
    | n > maxCoords = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
    | otherwise        = do
      let coords = [ (x, y) | x <- [minX..maxX-1], y <- [minY..maxY-1] ]
      shuffCoords <- fisherYatesShuffleM coords
      return $ take n shuffCoords
  where
    maxCoords = (maxX - minX) * (maxY - minY)