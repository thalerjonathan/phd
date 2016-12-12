module HACFrontend where

import Data.IORef
import qualified Graphics.Gloss as GLO

import HACSimulation as Sim
import HACAgent as Agent

winSizeX :: Int
winSizeX = 500

winSizeY :: Int
winSizeY = 500

agentSize :: Float
agentSize = 2

display :: GLO.Display
display = (GLO.InWindow "Heroes & Cowards IO (Gloss)" (winSizeX, winSizeY) (0, 0))

renderFrame :: Sim.SimIn -> IO GLO.Picture
renderFrame simIn = do
    pics <- mapM (renderAgent wt) as
    let p = GLO.Pictures $ pics
    return p
        where
            wt = Sim.simInWorldType simIn
            as = Sim.simInInitAgents simIn

renderAgent :: Agent.WorldType -> Agent.AgentState -> IO GLO.Picture
renderAgent wt as = do
    pos <- readIORef $ agentPos as
    let (relXCoord, relYCoord) = agentToGlossCoordinates $ wtf pos
    let x = fromRational (toRational relXCoord * ((fromIntegral winSizeX) / 2.0))
    let y = fromRational (toRational relYCoord * ((fromIntegral winSizeY) / 2.0))
    let pic = GLO.color color $ GLO.translate x y $ GLO.ThickCircle agentSize (2*agentSize)
    return pic
        where
            color = agentColor as
            wtf = worldTypeFunc wt

agentToGlossCoordinates :: AgentPosition -> AgentPosition
agentToGlossCoordinates (x, y) = (x', y')
    where
        x' = 2*x - 1.0
        y' = 2*y - 1.0

worldTypeFunc :: Agent.WorldType -> (AgentPosition -> AgentPosition)
worldTypeFunc wt
    | wt == InfiniteWraping = truncateToWorld
    | otherwise = id

truncateToWorld :: AgentPosition -> AgentPosition
truncateToWorld (x, y) = wrap (xFract, yFract)
    where
        xFract = fractionalPart x
        yFract = fractionalPart y

wrap :: AgentPosition -> AgentPosition
wrap (x, y) = (wrappedX, wrappedY)
    where
        wrappedX = wrapValue x
        wrappedY = wrapValue y

wrapValue :: Double -> Double
wrapValue v
    | v < 0.0 = v + 1.0
    | otherwise = v

fractionalPart :: Double -> Double
fractionalPart x = fractPart
    where
        (intPart, fractPart) = properFraction x

agentColor :: Agent.AgentState -> GLO.Color
agentColor a
    | hero a = GLO.green
    | otherwise = GLO.red
