module PolicyEffects.Renderer (
    renderFrame
  ) where

import PolicyEffects.Model

import FrABS.Agent.Agent
import FrABS.Env.Environment

import qualified Graphics.Gloss as GLO

renderFrame :: (Int, Int) 
                -> [PolicyEffectsAgentOut] 
                -> PolicyEffectsEnvironment 
                -> GLO.Picture
renderFrame wSize@(wx, wy) aouts env = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        maxWealth = foldr (\ao m -> if (aoState ao > m) then aoState ao else m) 0 aouts

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize maxWealth) aouts

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> Int
                -> PolicyEffectsAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) maxWealth a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColor maxWealth $ aoState a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

agentColor :: Int -> Int -> GLO.Color
agentColor maxWealth agentWealth = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac shade) 1.0 
    where
        shade = fromIntegral agentWealth / fromIntegral maxWealth