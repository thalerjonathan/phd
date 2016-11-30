module HACFrontend where

import Debug.Trace

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Control.Monad

import HACAgent as Agent

winSizeX :: GLsizei
winSizeX = 1920

winSizeY :: GLsizei
winSizeY = 1000

winSize :: GL.Size
winSize = (GL.Size winSizeX winSizeY)

green :: GL.Color3 GLdouble
green = greenShade 1.0

greenShade :: GLdouble -> GL.Color3 GLdouble
greenShade s = GL.Color3 0.0 s 0.0

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

renderFrame :: [Agent.AgentOut] -> IO Bool
renderFrame aos = do
    GL.clear [GL.ColorBuffer]
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    --GL.translate $ Vector3 100 100 (0.0 :: GLdouble)
    --GL.rotate (-30.0 :: GLdouble) $ Vector3 0.0 0.0 (1.0 :: GLdouble)
    mapM_ (\ao -> renderAgent ao ) aos
    GLFW.swapBuffers
    getParam Opened

agentSizeHalf :: Double
agentSizeHalf = 10.0

renderAgent :: Agent.AgentOut -> IO ()
renderAgent ao = preservingMatrix $ do
    GL.translate $ Vector3 xCoord yCoord 0
    GL.rotate angleDeg $ Vector3 0.0 0.0 1.0
    GL.renderPrimitive GL.Triangles agentTriangle
        where
            dir = agentOutDir ao
            angleRad = acos $ dotProd dir (1.0, 0.0)    -- NOTE: both vectors are of unit-length => must be the cos of the angle => acos for inverted results in radians,
            angleDeg = angleRad * radToDegFact          -- NOTE: transform radians to degree
            aState = agentOutState ao
            (relXCoord, relYCoord) = agentPos aState
            xCoord = relXCoord * fromIntegral winSizeX
            yCoord = relYCoord * fromIntegral winSizeY
            radToDegFact = (180.0/pi)

dotProd :: (Double, Double) -> (Double, Double) -> Double
dotProd (x1, y1) (x2, y2) = x1*x2 + y1*y2

agentTriangle :: IO ()
agentTriangle = do
    GL.color $ color
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
            color = greenShade 0.5