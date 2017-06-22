module Wildfire.WildfireRenderer where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import Wildfire.WildfireModel
import Wildfire.WildfireAgent

import qualified Graphics.Gloss as GLO

import Debug.Trace

display :: String -> (Int, Int) -> GLO.Display
display title winSize = (GLO.InWindow title winSize (300, 0))

renderFrame :: [WildfireAgentOut] -> WildfireEnvironment -> (Int, Int) -> GLO.Picture
renderFrame aouts env wSize@(wx, wy) = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> WildfireAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix  $ GLO.rectangleSolid rectWidth rectWidth -- $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColor $ aoState a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        
agentColor :: WildfireAgentState -> GLO.Color
agentColor WildfireAgentState { wfLifeState = state, wfFuel = fuel }
    | Living == state = GLO.makeColor (realToFrac 0.0) (realToFrac 0.9) (realToFrac 0.0) 1.0
    | Burning == state = GLO.makeColor (realToFrac fuel) (realToFrac 0.0) (realToFrac 0.0) 1.0
    | Dead == state = GLO.greyN 0.5 