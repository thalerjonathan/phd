module Init 
  (
    initWildfire
  ) where

import System.Random

import FRP.FrABS
import FRP.Yampa

import Model
import Agent

initWildfire :: RandomGen g => (Int, Int) -> Rand g ([WildfireAgentDef], WildfireEnvironment)
initWildfire dims@(maxX, maxY) = do
  let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
  let agentCount = maxX * maxY
  let aids = [0..agentCount-1]
  let aidCoordPairs = zip aids coords
  let cells = zip coords aids
  let centerX = floor $ (fromIntegral maxX) * 0.5
  let centerY = floor $ (fromIntegral maxY) * 0.5

  adefs <- mapM (wildfireAgent dims (centerX, centerY)) aidCoordPairs

  let e = createDiscrete2d
            dims
            neumann
            ClipToMax
            cells

  return (adefs, e)

wildfireAgent :: RandomGen g => Discrete2dDimension
                  -> Discrete2dCoord
                  -> (AgentId, Discrete2dCoord) 
                  -> Rand g WildfireAgentDef
wildfireAgent dims center aidCoord@(agentId, coord) = do
  rng <- newStdGen

  let initIgnite = center == coord
  let initMessages = if initIgnite then Event [(0, Ignite)] else NoEvent

  let sphereInitFuel = sphereFuelFunction dims center coord
  let boxInitFuel = boxFuelFunction dims 10 10 coord

  randInitFuel <- randomRIO randomFuelInitRange
  randFuelRate <- randomRIO randomFuelRateRange

  let initFuel = boxInitFuel

  let s = WildfireAgentState {
    wfLifeState = Living
  , wfFuelCurr = initFuel
  , wfFuelRate = randFuelRate
  , wfCoord = coord
  }

  return AgentDef {
    adId = agentId
  , adState = s
  , adConversation = Nothing
  , adInitMessages = initMessages
  , adBeh = wildfireAgentBehaviour rng initFuel
  , adRng = rng 
  }

sphereFuelFunction :: Discrete2dDimension -> (Int, Int) -> Discrete2dCoord -> Double
sphereFuelFunction (dimX, dimY) (centerX, centerY) (x, y) = 1 - (max r 0)
  where
    x' = x - centerX
    y' = y - centerY

    xNorm = fromIntegral x' / fromIntegral dimX
    yNorm = fromIntegral y' / fromIntegral dimY

    r = sqrt $ (xNorm^2) + (yNorm^2)

boxFuelFunction :: Discrete2dDimension -> Int -> Int -> Discrete2dCoord -> Double
boxFuelFunction (dimX, dimY) horizontal vertical (x, y)
  | x > horizontal && y > vertical = 1.0
  | otherwise = 0.5