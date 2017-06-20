module PrisonersDilemma.PDRenderer where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import PrisonersDilemma.PDModel
import PrisonersDilemma.PDAgent

import qualified Graphics.Gloss as GLO

import Debug.Trace

display :: String -> (Int, Int) -> GLO.Display
display title winSize = (GLO.InWindow title winSize (300, 0))

renderFrame :: [PDAgentOut] -> PDEnvironment -> (Int, Int) -> GLO.Picture
renderFrame aouts env wSize@(wx, wy) = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts


renderAgent :: (Float, Float)
                -> (Int, Int)
                -> PDAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix  $ GLO.rectangleSolid rectWidth rectWidth -- $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColor $ aoState a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        
agentColor :: PDAgentState -> GLO.Color
agentColor PDAgentState { pdCurrAction = curr, pdPrevAction = prev } = agentActionsToColor prev curr

agentActionsToColor :: PDAction -> PDAction -> GLO.Color
agentActionsToColor Cooperator Cooperator = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0
agentActionsToColor Defector Defector = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
agentActionsToColor Defector Cooperator = GLO.makeColor (realToFrac 0.0) (realToFrac 0.4) (realToFrac 0.0) 1.0
agentActionsToColor Cooperator Defector = GLO.makeColor (realToFrac 1.0) (realToFrac 0.9) (realToFrac 0.0) 1.0
