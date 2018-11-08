module SugarScape.Init 
  ( createSugarScape
  ) where

import Control.Monad.Random

import Data.Char
import Data.List

import SugarScape.Agent
import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random

createSugarScape :: RandomGen g
                 => SugarScapeParams
                 -> Rand g ([(AgentId, SugAgentObservable, SugAgentMSF g)], SugEnvironment)
createSugarScape params = do
  ras <- agentDistribution params (sgAgentDistribution params)

  let as          = map (\(ad, _) -> (adId ad, adInitObs ad, adSf ad)) ras
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

agentDistribution :: RandomGen g
                  => SugarScapeParams
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

changeToRedTribe :: SugarScapeParams
                 -> SugAgentState
                 -> SugAgentState
changeToRedTribe params s = s { sugAgTribe      = tagToTribe redTag
                              , sugAgCultureTag = redTag }
  where             
    redTag = case spCulturalProcess params of 
              Nothing -> []
              Just n  -> replicate n True

changeToBlueTribe :: SugarScapeParams
                  -> SugAgentState
                  -> SugAgentState
changeToBlueTribe params s = s { sugAgTribe     = tagToTribe blueTag
                              , sugAgCultureTag = blueTag }
  where             
    blueTag = case spCulturalProcess params of 
              Nothing -> []
              Just n  -> replicate n False

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
    occ         = maybe Nothing (\(_, (aid, s)) -> (Just $ occupier aid s)) mayOccupier

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