module SugarScape.Init 
  ( createSugarScape
  ) where

import Control.Monad.Random

import Data.Char
import Data.List

import SugarScape.Agent
import SugarScape.AgentMonad
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random

createSugarScape :: RandomGen g
                 => SugarScapeParams
                 -> Rand g ([(AgentId, SugAgent g)], SugEnvironment)
createSugarScape params = do
  let agentCount = sgAgentCount params
      agentDistr = sgAgentDistribution params
      ais        = [1..agentCount]

  let coordDims  = case agentDistr of
                    Scatter      -> sugarscapeDimensions
                    (Corner dim) -> dim

  randCoords <- randomCoords (0,0) coordDims agentCount
  ras        <- mapM (\(aid, coord) -> randomAgent params (aid, coord) (agentSF params) id) (zip ais randCoords)

  let as          = map (\(aid, (ad, _)) -> (aid, adSf ad)) (zip ais ras)
      occupations = map (\(ad, s) -> (sugAgCoord s, (adId ad, s))) ras
      sugSpecs    = parseSugarSpec sugarEnvSpec
      sugCoords   = sugarSpecToCoords sugSpecs sugarscapeDimensions
      sites       = createSites sugCoords occupations
      env         = createDiscrete2d
                      sugarscapeDimensions
                      neumann
                      WrapBoth
                      sites

  return (as, env)

sugarSpecToCoords :: [[Int]]
                  -> Discrete2dCoord
                  -> [(Discrete2dCoord, Int)]
sugarSpecToCoords specs (dimX, dimY) 
    | length specs /= dimY = error ("sugar row count does not match y-dimensions: " ++ show (length specs) ++ " not " ++ show dimY)
    | otherwise            = sugarSpecLines specs 0 []
  where
    sugarSpecLines :: [[Int]]
                   -> Int
                   -> [(Discrete2dCoord, Int)]
                   -> [(Discrete2dCoord, Int)]
    sugarSpecLines [] _ acc       = acc
    sugarSpecLines (l : ls) y acc 
        | length l /= dimX = error ("sugar spec line size does not match x-dimensions: " ++ show (length l) ++ " not " ++ show dimX)
        | otherwise        = sugarSpecLines ls (y + 1) acc'
      where
        acc' = sugarSpecLineToCoords l 0 acc

        sugarSpecLineToCoords :: [Int]
                              -> Int
                              -> [(Discrete2dCoord, Int)]
                              -> [(Discrete2dCoord, Int)]
        sugarSpecLineToCoords [] _ accLine       = accLine
        sugarSpecLineToCoords (s : ss) x accLine = sugarSpecLineToCoords ss (x + 1) accLine'
          where
            accLine' = ((x, y), s) : accLine

parseSugarSpec :: [String]
               -> [[Int]]
parseSugarSpec = map parseSugarSpecLine
  where
    parseSugarSpecLine :: String 
                       -> [Int]
    parseSugarSpecLine line0 = reverse $ parseSugarSpecAux line0 []
      where
        parseSugarSpecAux :: String 
                          -> [Int]
                          -> [Int]
        parseSugarSpecAux [] acc = reverse acc
        parseSugarSpecAux (c : cs) acc 
          | isNumber c = parseSugarSpecAux cs (digitToInt c : acc)
          | otherwise  = error "bad character in sugar specification"

createSites :: [(Discrete2dCoord, Int)]
            -> [(Discrete2dCoord, (AgentId, SugAgentState))]
            -> [(Discrete2dCoord, SugEnvSite)]
createSites siteSpecs occupations 
  = map (initRandomSite occupations) siteSpecs
 
initRandomSite :: [(Discrete2dCoord, (AgentId, SugAgentState))] 
               -> (Discrete2dCoord, Int) 
               -> (Discrete2dCoord, SugEnvSite)
initRandomSite os (coord, sugar) = (coord, c)
  where
    mayOccupier = Data.List.find ((==coord) . fst) os
    occ         = maybe Nothing (\(_, (aid, s)) -> (Just (siteOccupier aid s))) mayOccupier

    c = SugEnvSite {
      sugEnvSiteSugarCapacity = fromIntegral sugar
    , sugEnvSiteSugarLevel    = fromIntegral sugar
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