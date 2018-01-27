{-# LANGUAGE Arrows     #-}
module Main where

import System.IO

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.MSF.Random
import Data.Array.IArray
import FRP.BearRiver
import qualified Graphics.Gloss as GLO
import qualified Graphics.Gloss.Interface.IO.Game as GLOGame

import SIR

type Disc2dCoord  = (Int, Int)
type SIREnv       = Array Disc2dCoord SIRState
type SIRMonad g   = StateT SIREnv (Rand g)
type SIRAgent g   = SF (SIRMonad g) () ()

agentGridSize :: (Int, Int)
agentGridSize = (21, 21)

rngSeed :: Int
rngSeed = 123

dt :: DTime
dt = 0.1

t :: Time
t = 200

winSize :: (Int, Int)
winSize = (600, 600)

winTitle :: String
winTitle = "Agent-Based SIR on 2D Grid"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g         = mkStdGen rngSeed
      (as, e)   = initAgentsEnv agentGridSize

      es        = runSimulation g t dt e as

      ass       = environmentsToAgentDyns es
      dyns      = aggregateAllStates ass
      fileName  =  "STEP_5_ENVIRONMENT_DYNAMICS_" ++ show agentGridSize ++ "agents.m"
  
  writeAggregatesToFile fileName dyns
  render es

environmentsToAgentDyns :: [SIREnv] -> [[SIRState]]
environmentsToAgentDyns = map elems

render :: [SIREnv] -> IO ()
render es = GLOGame.playIO
    (GLO.InWindow winTitle winSize (0, 0))
    GLO.white
    10
    (length es - 1, False, False, False)
    worldToPic
    handleInput
    stepWorld

  where
    (cx, cy) = agentGridSize
    (wx, wy) = winSize
    cellWidth = (fromIntegral wx / fromIntegral cx) :: Double
    cellHeight = (fromIntegral wy / fromIntegral cy) :: Double

    worldToPic :: (Int, Bool, Bool, Bool) -> IO GLO.Picture
    worldToPic (i, _, _, _) = do
      let e = es !! i
      let as = assocs e
      let aps = map renderAgent as

      let (tcx, tcy) = transformToWindow (0, -2)
      let timeTxt = "t = " ++ show (fromIntegral i * dt)
      let timeStepTxt = GLO.color GLO.black $ GLO.translate tcx tcy $ GLO.scale 0.1 0.1 $ GLO.Text timeTxt
      
      return $ GLO.Pictures $ aps ++ [timeStepTxt]

    handleInput :: GLOGame.Event -> (Int, Bool, Bool, Bool) -> IO (Int, Bool, Bool, Bool)
    handleInput evt w@(i, inc, dec, boost) = 
      case evt of
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyLeft) GLOGame.Down _ _) -> return (i, inc, True, boost)
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyRight) GLOGame.Down _ _) -> return (i, True, dec, boost)
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyShiftL) GLOGame.Down _ _) -> return (i, inc, dec, True)
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyLeft) GLOGame.Up _ _) -> return (i, inc, False, boost)
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyRight) GLOGame.Up _ _) -> return (i, False, dec, boost)
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyShiftL) GLOGame.Up _ _) -> return (i, inc, dec, False)

        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyDown) GLOGame.Down _ _) -> return (0, inc, dec, boost)
        (GLOGame.EventKey (GLOGame.SpecialKey GLOGame.KeyUp) GLOGame.Down _ _) -> return (length es - 1, inc, dec, boost)
        _ -> return w

    stepWorld :: Float -> (Int, Bool, Bool, Bool) -> IO (Int, Bool, Bool, Bool)
    stepWorld _ (i, inc, dec, boost) = do
      let delta = if boost then 10 else 1

      let i' = if inc then min (length es - 1) (i+delta) else i
      let i'' = if dec then max 0 (i'-delta) else i'
      return (i'', inc, dec, boost)

    renderAgent :: (Disc2dCoord, SIRState) -> GLO.Picture
    renderAgent (coord, s) = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 (realToFrac cellWidth)
      where
        color = sirColor s
        (x, y) = transformToWindow coord

    sirColor :: SIRState -> GLO.Color
    sirColor Susceptible  = GLO.makeColor 0.0 0.0 0.7 1.0 
    sirColor Infected     = GLO.makeColor 0.7 0.0 0.0 1.0
    sirColor Recovered    = GLO.makeColor 0.0 0.55 0.0 1.0
    
    transformToWindow :: Disc2dCoord -> (Float, Float)
    transformToWindow (x, y) = (x', y')
      where
        rw = cellWidth
        rh = cellHeight

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        x' = fromRational (toRational (fromIntegral x * rw)) - halfXSize
        y' = fromRational (toRational (fromIntegral y * rh)) - halfYSize

initAgentsEnv :: (Int, Int) -> ([(Disc2dCoord, SIRState)], SIREnv)
initAgentsEnv (xd, yd) = (as, e)
  where
    xCenter = floor $ fromIntegral xd * (0.5 :: Double)
    yCenter = floor $ fromIntegral yd * (0.5 :: Double)
    
    sus = [ ((x, y), Susceptible) | x <- [0..xd-1], 
                                    y <- [0..yd-1],
                                    x /= xCenter ||
                                    y /= yCenter ] 
    inf = ((xCenter, yCenter), Infected)
    as = inf : sus

    e = array ((0, 0), (xd - 1, yd - 1)) as

runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> SIREnv
              -> [(Disc2dCoord, SIRState)] 
              -> [SIREnv]
runSimulation g t dt e as = es
  where
    steps = floor $ t / dt
    dts = replicate steps ()
    sfs = map (uncurry sirAgent) as

    esReader = embed (stepSimulation sfs) dts
    esState = runReaderT esReader dt
    esRand = evalStateT esState e
    es = evalRand esRand g

stepSimulation :: RandomGen g
               => [SIRAgent g]
               -> SF (SIRMonad g) () SIREnv
stepSimulation sfs = MSF $ \_ -> do
  res <- mapM (`unMSF` ()) sfs
  let sfs'  = fmap snd res
  e <- get
  let ct = stepSimulation sfs'
  return (e, ct)

sirAgent :: RandomGen g => Disc2dCoord -> SIRState -> SIRAgent g
sirAgent c Susceptible = susceptibleAgent c
sirAgent c Infected    = infectedAgent c
sirAgent _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => Disc2dCoord -> SIRAgent g
susceptibleAgent coord = 
    switch 
      susceptible
      (const $ infectedAgent coord)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) () ((), Event ())
    susceptible = proc _ -> do
      makeContact <- occasionallyM (1 / contactRate) () -< ()

      if not $ isEvent makeContact 
        then returnA -< ((), NoEvent)
        else (do
          e <- arrM_ (lift get) -< ()
          let ns = neighbours e coord agentGridSize moore
          --let ns = allNeighbours e
          s <- drawRandomElemS       -< ns

          if Infected /= s
            then returnA -< ((), NoEvent)
            else (do
              infected <- arrM_ (lift $ lift $ randomBoolM infectivity) -< ()
              if infected 
                then (do
                  arrM (put . changeCell coord Infected) -< e
                  returnA -< ((), Event ()))
                else returnA -< ((), NoEvent)))

infectedAgent :: RandomGen g => Disc2dCoord -> SIRAgent g
infectedAgent coord = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) () ((), Event ())
    infected = proc _ -> do
      recovered <- occasionallyM illnessDuration () -< ()
      if isEvent recovered
        then (do
          e <- arrM (\_ -> lift get) -< ()
          arrM (put . changeCell coord Recovered) -< e
          returnA -< ((), Event ()))
        else returnA -< ((), NoEvent)

recoveredAgent :: RandomGen g => SIRAgent g
recoveredAgent = arr (const ())

drawRandomElemS :: MonadRandom m => SF m [a] a
drawRandomElemS = proc as -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  let len = length as
  let idx = fromIntegral len * r
  let a =  as !! floor idx
  returnA -< a

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

changeCell :: Disc2dCoord -> SIRState -> SIREnv -> SIREnv
changeCell c s e = e // [(c, s)]

neighbours :: SIREnv 
           -> Disc2dCoord 
           -> Disc2dCoord
           -> [Disc2dCoord] 
           -> [SIRState]
neighbours e (x, y) (dx, dy) n = map (e !) nCoords'
  where
    nCoords = map (\(x', y') -> (x + x', y + y')) n
    nCoords' = filter (\(x, y) -> x >= 0 && 
                                  y >= 0 && 
                                  x <= (dx - 1) &&
                                  y <= (dy - 1)) nCoords
allNeighbours :: SIREnv -> [SIRState]
allNeighbours = elems

neumann :: [Disc2dCoord]
neumann = [ topDelta, leftDelta, rightDelta, bottomDelta ]

moore :: [Disc2dCoord]
moore = [ topLeftDelta,    topDelta,     topRightDelta,
          leftDelta,                     rightDelta,
          bottomLeftDelta, bottomDelta,  bottomRightDelta ]

topLeftDelta :: Disc2dCoord
topLeftDelta      = (-1, -1)
topDelta :: Disc2dCoord
topDelta          = ( 0, -1)
topRightDelta :: Disc2dCoord
topRightDelta     = ( 1, -1)
leftDelta :: Disc2dCoord
leftDelta         = (-1,  0)
rightDelta :: Disc2dCoord
rightDelta        = ( 1,  0)
bottomLeftDelta :: Disc2dCoord
bottomLeftDelta   = (-1,  1)
bottomDelta :: Disc2dCoord
bottomDelta       = ( 0,  1)
bottomRightDelta :: Disc2dCoord
bottomRightDelta  = ( 1,  1)