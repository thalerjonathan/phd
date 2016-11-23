module HACFrontend where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Control.Monad

import HACBackend as Back

winSizeX :: GLsizei
winSizeX = 800

winSizeY :: GLsizei
winSizeY = 800

winSize :: GL.Size
winSize = (GL.Size winSizeX winSizeY)

winDimXInt :: GL.Size -> Int
winDimXInt (Size x y) = fromIntegral x

winDimYInt :: GL.Size -> Int
winDimYInt (Size x y) = fromIntegral y

green :: GL.Color3 GLdouble
green = GL.Color3 0.0 1.0 0.0

black :: GL.Color3 GLdouble
black = GL.Color3 0.0 0.0 0.0

greenShade :: GLdouble -> GL.Color3 GLdouble
greenShade s = GL.Color3 0.0 s 0.0

redShade :: GLdouble -> GL.Color3 GLdouble
redShade s = GL.Color3 s 0.0 0.0

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

{-
  GLFW.windowCloseCallback $= \flag ->
    do
        GLFW.terminate
        return True
-}

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

renderFrame :: [Back.AgentPosition] -> IO ()
renderFrame agents = do
    GL.clear [GL.ColorBuffer]
    GL.renderPrimitive GL.Triangles $ mapM_ (\a -> renderAgent a ) agents
    GLFW.swapBuffers

agentSizeHalf :: Double
agentSizeHalf = 10.0

renderAgent :: Back.AgentPosition -> IO ()
renderAgent agent = do
    GL.color $ color
    GL.vertex top
    GL.vertex bottomRight
    GL.vertex bottomLeft
    where
            xCoord = fst agent
            yCoord = snd agent
            xRight = xCoord + agentSizeHalf
            xLeft = xCoord - agentSizeHalf
            yTop = yCoord + agentSizeHalf
            yBottom = yCoord - agentSizeHalf
            top = GL.Vertex3 xCoord yTop 0.0
            bottomRight = GL.Vertex3 xRight yBottom 0.0
            bottomLeft = GL.Vertex3 xLeft yBottom 0.0
            color = greenShade 0.5