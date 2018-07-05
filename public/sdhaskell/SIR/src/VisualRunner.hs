module VisualRunner 
  (
    runSDVisual
  ) where

import           Data.IORef
import           Text.Printf

import           FRP.Yampa
import qualified Graphics.Gloss as GLO
import           Graphics.Gloss.Interface.IO.Animate
--import           Graphics.Gloss.Interface.IO.Simulate

import           SIR

runSDVisual :: Double
            -> Double
            -> Double
            -> Double
            -> Double
            -> Time
            -> DTime
            -> IO ()
runSDVisual populationSize infectedCount contactRate infectivity illnessDuration tLimit dt = do 
    ref <- newIORef initSir

    hdl <- reactInit 
            (return ()) 
            (stepSIR ref)
            (sir populationSize infectedCount contactRate infectivity illnessDuration)

    pathRef <- newIORef ([], [], [])
{-
    simulateIO
      (GLO.InWindow "SIR System Dynamics" winSize (0, 0))
      white                                 -- background
      10000                -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
      (initSir, 0)                   -- initial model = output of each simulation step to be rendered
      (modelToPicture tLimit populationSize pathRef)  -- model-to-picture function
      (renderStepSimulate tLimit dt hdl ref)    -- 
-}

    modelRef <- newIORef (0, GLO.Blank)

    animateIO
        (GLO.InWindow "SIR System Dynamics" winSize (0, 0))
        white
        (renderStepAnimate populationSize tLimit dt hdl ref modelRef pathRef)
        (const $ return ())

  where
    winSize = (800, 800) 
    initSir = (populationSize - infectedCount, infectedCount, 0)

stepSIR :: IORef SIRStep
        -> ReactHandle () SIRStep
        -> Bool 
        -> SIRStep
        -> IO Bool
stepSIR ref _ _ out = do
  writeIORef ref out
  return True

{-
renderStepSimulate :: Time
                   -> DTime
                   -> ReactHandle () SIRStep
                   -> IORef SIRStep
                   -> ViewPort
                   -> Float
                   -> (SIRStep, Time)
                   -> IO (SIRStep, Time)
renderStepSimulate tLimit dt hdl ref _ _ (out, t) = do
  let t' = t + dt

  if t' >= tLimit
    then return (out, t)
    else do
      _    <- react hdl (dt, Nothing)
      out' <- readIORef ref
      return (out', t')
-}

renderStepAnimate :: Double
                  -> Time
                  -> DTime
                  -> ReactHandle () SIRStep
                  -> IORef SIRStep
                  -> IORef (Time, GLO.Picture)
                  -> IORef (GLO.Path, GLO.Path, GLO.Path)
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate populationSize tLimit dt hdl sirOutRef modelRef pathRef _ = do
  (t, pic) <- readIORef modelRef

  let t' = t + dt

  if t' >= tLimit
    then return pic
    else do
      _    <- react hdl (dt, Nothing)
      out  <- readIORef sirOutRef
      pic' <- modelToPicture tLimit populationSize pathRef (out, t')
      _    <- writeIORef modelRef (t', pic')

      return pic'

modelToPicture :: Time
               -> Double
               -> IORef (GLO.Path, GLO.Path, GLO.Path)
               -> (SIRStep, Time)
               -> IO GLO.Picture
modelToPicture tLimit populationSize pathRef out@((sus, inf, reco), t) = do
    linesPic <- renderLines tLimit populationSize out pathRef

    let (xTxt, yTxt) = (-50, 100)
        timeTxt      = GLO.color GLO.black $ 
                       GLO.translate xTxt yTxt $ 
                       GLO.scale 0.2 0.2 $ 
                       GLO.Text $ printf "t = %.2f" t

        (xSus, ySus)  = (-100, 0)
        susStockScale = GLO.scale 1 $ realToFrac (sus / populationSize)
        susStock      = GLO.color GLO.blue $ 
                        GLO.translate xSus ySus $ 
                        susStockScale $
                        stockPolySize 
                        stockPoly
        susLine       = GLO.color GLO.black $ 
                        GLO.translate xSus ySus $ 
                        stockPolySize 
                        stockLine
        susStockTxt = GLO.color GLO.black $ 
                      GLO.translate xSus (ySus - 15) $ 
                      GLO.scale 0.1 0.1 $ 
                      GLO.Text $ printf "%.2f" sus

        (xInf, yInf)  = (0, 0)
        infStockScale = GLO.scale 1 $ realToFrac (inf / populationSize)
        infStock      = GLO.color GLO.red $ 
                        GLO.translate xInf yInf $ 
                        infStockScale $
                        stockPolySize 
                        stockPoly
        infLine       = GLO.color GLO.black $ 
                        GLO.translate xInf yInf $ 
                        stockPolySize 
                        stockLine
        infStockTxt = GLO.color GLO.black $ 
                      GLO.translate xInf (yInf - 15) $ 
                      GLO.scale 0.1 0.1 $ 
                      GLO.Text $ printf "%.2f" inf

        (xRec, yRec)  = (100, 0)
        recStockScale = GLO.scale 1 $ realToFrac (reco / populationSize)
        recStock      = GLO.color GLO.green $ 
                        GLO.translate xRec yRec $ 
                        recStockScale $
                        stockPolySize 
                        stockPoly
        recLine       = GLO.color GLO.black $ 
                        GLO.translate xRec yRec $ 
                        stockPolySize 
                        stockLine
        recStockTxt = GLO.color GLO.black $ 
                      GLO.translate xRec (yRec - 15) $ 
                      GLO.scale 0.1 0.1 $ 
                      GLO.Text $ printf "%.2f" reco

    return $ GLO.Pictures [timeTxt, 
                           susStock, susStockTxt, susLine,
                           infStock, infStockTxt, infLine,
                           recStock, recStockTxt, recLine,
                           linesPic]
  where
    stockPolySize = GLO.scale 50 50
    stockPath     = [(0, 0), (1, 0), (1, 1), (0, 1)]
    stockPoly     = GLO.Polygon stockPath
    stockLine     = GLO.Line $ (0, 1) : stockPath

renderLines :: Time
            -> Double
            -> (SIRStep, Time)
            -> IORef (GLO.Path, GLO.Path, GLO.Path)
            -> IO GLO.Picture
renderLines tLimit populationSize ((sus, inf, reco), t) pathRef = do
    (susPath, infPath, recPath) <- readIORef pathRef

    let tFract   = realToFrac $ t / tLimit
        susPoint = (tFract, realToFrac (sus / populationSize))
        infPoint = (tFract, realToFrac (inf / populationSize))
        recPoint = (tFract, realToFrac (reco / populationSize))

        -- could exploit 
        susPath' = susPoint : susPath
        infPath' = infPoint : infPath
        recPath' = recPoint : recPath

        susLine  = GLO.color GLO.blue $ 
                   GLO.translate lineOriginX lineOriginY $ 
                   lineScale $ 
                   GLO.Line susPath'
        infLine  = GLO.color GLO.red $ 
                   GLO.translate lineOriginX lineOriginY $ 
                   lineScale $ 
                   GLO.Line infPath'
        recLine  = GLO.color GLO.green $ 
                   GLO.translate lineOriginX lineOriginY $ 
                   lineScale $ 
                   GLO.Line recPath'

    writeIORef pathRef (susPath', infPath', recPath')

    return $ GLO.Pictures [susLine, infLine, recLine] 
  where
    lineScale   = GLO.scale 300 200
    lineOriginX = -150
    lineOriginY = -250