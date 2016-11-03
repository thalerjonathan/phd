module WildFireFrontend where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad

import WildFireBackend as Back

winSizeX :: GLsizei
winSizeX = 400

winSizeY :: GLsizei
winSizeY = 400

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
  GLFW.windowTitle $= "GLFW Demo"
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

renderFrame :: [Back.Cell] -> (Int, Int) -> IO ()
renderFrame cells (dimX, dimY) = do
    GL.clear [GL.ColorBuffer]
    GL.renderPrimitive GL.Quads $ mapM_ (\c -> renderCell c cellDimPixels ) cells
    GLFW.swapBuffers
    where
        cellDimPixels = cellDimensions (dimX, dimY)

renderCell :: Back.Cell -> (GLdouble, GLdouble) -> IO ()
renderCell cell (cellWidth, cellHeight) = do
    GL.color $ color
    GL.vertex topLeft
    GL.vertex topRight
    GL.vertex bottomRight
    GL.vertex bottomLeft
    where
            (xIdx, yIdx) = cellCoord cell
            xCoord = (cellWidth * fromIntegral xIdx) :: GLdouble
            yCoord = (cellHeight * fromIntegral yIdx) :: GLdouble
            topLeft = GL.Vertex3 xCoord yCoord 0.0
            topRight = GL.Vertex3 (xCoord + cellWidth) yCoord 0.0
            bottomLeft = GL.Vertex3 xCoord (yCoord + cellHeight) 0.0
            bottomRight = GL.Vertex3 (xCoord + cellWidth) (yCoord + cellHeight) 0.0
            color = cellColor cell

cellColor :: Back.Cell -> GL.Color3 GLdouble
cellColor cell
    | state == LIVING = greenShade fuel
    | state == BURNING = redShade fuel
    | state == DEAD = black
    where
        state = cellState cell
        fuel = cellFuel cell

cellDimensions :: (Int, Int) -> (GLdouble, GLdouble)
cellDimensions (dimX, dimY) = (cellWidth, cellHeight)
     where
            cellWidth = ( fromIntegral (winDimXInt winSize) / fromIntegral dimX ) :: GLdouble
            cellHeight = ( fromIntegral (winDimYInt winSize) / fromIntegral dimY ) :: GLdouble

checkMousePress :: IO (Maybe (Int, Int))
checkMousePress = do
    b <- GLFW.getMouseButton GLFW.ButtonLeft
    case b of
        GLFW.Press -> do
            (GL.Position x y) <- GL.get GLFW.mousePos
            return (Just (fromIntegral x, fromIntegral y))
        otherwise -> return Nothing

pixelCoordToCellIdx :: (Int, Int) -> (Int, Int) -> IO Int
pixelCoordToCellIdx (xDim, yDim) (xCoord, yCoord) = do
    putStrLn ("coords = " ++ show x ++ "/" ++ show y)
    putStrLn ("indices = " ++ show xIdx ++ "/" ++ show yIdx)
    putStrLn ("idx = " ++ show idx)
    return idx
    where
        x = fromIntegral xCoord
        y = fromIntegral yCoord
        xIdx = floor (x / cellWidth)
        yIdx = floor (y / cellHeight)
        (cellWidth, cellHeight) = cellDimensions (xDim, yDim)
        idx = (yIdx * xDim) + xIdx