module HACFrontend where

import Debug.Trace

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Control.Monad

import HACAgent as Agent

winSizeX :: GLsizei
winSizeX = 800

winSizeY :: GLsizei
winSizeY = 800

winSize :: GL.Size
winSize = (GL.Size winSizeX winSizeY)

green :: GL.Color3 GLdouble
green = greenShade 1.0

greenShade :: GLdouble -> GL.Color3 GLdouble
greenShade s = GL.Color3 0.0 s 0.0

redShade :: GLdouble -> GL.Color3 GLdouble
redShade s = GL.Color3 s 0.0 0.0

agentSizeHalf :: Double
agentSizeHalf = 10.0

agentQuadSizeHalf :: Double
agentQuadSizeHalf = 2

agentTailSize :: Double
agentTailSize = 10.0

initialize :: IO ()
initialize = do
  GLFW.initialize
  -- open window
  GLFW.openWindow winSize [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Heros & Cowards"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 1 1 1 0
  -- set the point-size
  GL.pointSize  $= realToFrac agentTailSize
  GL.pointSmooth $= Enabled
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

shutdown :: IO ()
shutdown = do
  GLFW.closeWindow
  GLFW.terminate

renderFrame :: [Agent.AgentOut] -> Agent.WorldType -> IO Bool
renderFrame aos wt = do
    GL.clear [GL.ColorBuffer]
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    mapM (renderAgent wt) aos
    GLFW.swapBuffers
    getParam Opened

renderAgent :: Agent.WorldType -> Agent.AgentOut -> IO ()
renderAgent = renderAgentQuad

renderAgentQuad :: Agent.WorldType -> Agent.AgentOut -> IO ()
renderAgentQuad wt ao = preservingMatrix $ do
    GL.color $ color
    GL.translate $ Vector3 xCoord yCoord 0
    GL.renderPrimitive GL.Quads agentQuad
        where
            aState = agentOutState ao
            (relXCoord, relYCoord) = agentPos aState
            xCoord = relXCoord * fromIntegral winSizeX
            yCoord = relYCoord * fromIntegral winSizeY
            color = agentColor aState


renderAgentWithOrientation :: Agent.WorldType -> Agent.AgentOut -> IO ()
renderAgentWithOrientation wt ao = preservingMatrix $ do
    GL.color $ color
    GL.translate $ Vector3 xCoord yCoord 0
    GL.rotate angleDeg $ Vector3 0.0 0.0 1.0
    GL.renderPrimitive GL.Triangles agentTriangle
    GL.renderPrimitive GL.Points agentPoint
        where
            (dirX, dirY) = agentOutDir ao
            angleRad = atan2 dirX dirY                    -- NOTE: to get the angle of a 2D-vector in radians, use atan2
            angleDeg = (pi - angleRad) * radToDegFact     -- NOTE: because the coordinate-systems y-achsis is pointing downwards, we need to adjust the angle
            aState = agentOutState ao
            (relXCoord, relYCoord) = agentPos aState
            xCoord = relXCoord * fromIntegral winSizeX
            yCoord = relYCoord * fromIntegral winSizeY
            radToDegFact = (180.0/pi)
            color = agentColor aState

agentColor :: Agent.AgentState -> GL.Color3 GLdouble
agentColor a
    | hero a = greenShade 1.0
    | otherwise = redShade 1.0

agentPoint :: IO ()
agentPoint = do
    GL.vertex $ GL.Vertex3 0.0 distance 0.0
        where
            distance = agentSizeHalf + (agentTailSize / 2.0)

agentQuad :: IO ()
agentQuad = do
    GL.vertex topLeft
    GL.vertex topRight
    GL.vertex bottomRight
    GL.vertex bottomLeft
        where
            xRight = agentQuadSizeHalf
            xLeft = -agentQuadSizeHalf
            yTop = -agentQuadSizeHalf
            yBottom = agentQuadSizeHalf
            topLeft = GL.Vertex3 xLeft yTop 0.0
            topRight = GL.Vertex3 xRight yTop 0.0
            bottomRight = GL.Vertex3 xRight yBottom 0.0
            bottomLeft = GL.Vertex3 xLeft yBottom 0.0

agentTriangle :: IO ()
agentTriangle = do
    GL.vertex top
    GL.vertex bottomRight
    GL.vertex bottomLeft
        where
            xRight = agentSizeHalf
            xLeft = -agentSizeHalf
            yTop = -agentSizeHalf
            yBottom = agentSizeHalf
            top = GL.Vertex3 0.0 yTop 0.0
            bottomRight = GL.Vertex3 xRight yBottom 0.0
            bottomLeft = GL.Vertex3 xLeft yBottom 0.0