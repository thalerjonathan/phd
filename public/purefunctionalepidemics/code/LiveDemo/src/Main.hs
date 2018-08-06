{-# LANGUAGE Arrows     #-}
module Main where

import           System.IO
import           Text.Printf

import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.MSF.Random
import           Data.Array.IArray
import           FRP.BearRiver
import qualified Graphics.Gloss as GLO
import qualified Graphics.Gloss.Interface.IO.Game as GLOGame

type Disc2dCoord  = (Int, Int)
type SIREnv       = Array Disc2dCoord SIRState
type SIRMonad g   = StateT SIREnv (Rand g)
type SIRAgent g   = SF (SIRMonad g) () ()

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

type SimSF g = SF (SIRMonad g) () SIREnv

data SimCtx g = SimCtx 
  { simSf    :: SimSF g
  , simEnv   :: SIREnv
  , simRng   :: g
  , simSteps :: Integer
  , simInput :: (Int, Bool, Bool, Bool)
  , simTime  :: Time
  }

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

agentGridSize :: (Int, Int)
agentGridSize = (11, 11)

winSize :: (Int, Int)
winSize = (600, 600)

winTitle :: String
winTitle = "Agent-Based SIR on 2D Grid"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let dt        = 0.1
      rngSeed   = 42 -- 123 -- 42 leads to recovery without any infection
      
      g         = mkStdGen rngSeed
      (as, env) = initAgentsEnv agentGridSize
      sfs       = map (uncurry sirAgent) as
      sf        = stepSimulation sfs
      simCtx    = mkSimCtx sf env g 0 0

  runSimulation dt simCtx

runSimulation :: RandomGen g
              => DTime
              -> SimCtx g
              -> IO ()
runSimulation dt simCtx 
  = GLOGame.playIO
      (GLO.InWindow winTitle winSize (0, 0))
      GLO.white
      5
      simCtx
      worldToPic
      handleInput
      stepWorld

  where
    (cx, cy)   = agentGridSize
    (wx, wy)   = winSize
    cellWidth  = (fromIntegral wx / fromIntegral cx) :: Double
    cellHeight = (fromIntegral wy / fromIntegral cy) :: Double

    worldToPic :: RandomGen g
               => SimCtx g 
               -> IO GLO.Picture
    worldToPic ctx = do
      let env = simEnv ctx
          as  = assocs env
          aps = map renderAgent as
          t   = simTime ctx

          (tcx, tcy) = transformToWindow (0, -2)
          timeTxt = "t = " ++ printf "%0.2f" t
          timeStepTxt = GLO.color GLO.black $ GLO.translate tcx tcy $ GLO.scale 0.5 0.5 $ GLO.Text timeTxt
      
      return $ GLO.Pictures $ aps ++ [timeStepTxt]

    handleInput :: RandomGen g
                => GLOGame.Event 
                -> SimCtx g
                -> IO (SimCtx g)
    handleInput _ = return 

    stepWorld :: RandomGen g
              => Float 
              -> SimCtx g
              -> IO (SimCtx g)
    stepWorld _  ctx = do
      let env                  = simEnv ctx
          g                    = simRng ctx

          sf                   = simSf ctx
          sfReader             = unMSF sf ()
          sfState              = runReaderT sfReader dt
          sfRand               = runStateT sfState env
          (((_, simSf'), env'), g') = runRand sfRand g
      
          steps   = simSteps ctx + 1
          t       = simTime ctx + dt
          ctx'    = mkSimCtx simSf' env' g' steps t

      return ctx'

    renderAgent :: (Disc2dCoord, SIRState) -> GLO.Picture
    renderAgent (coord, Susceptible) 
        = GLO.color (GLO.makeColor 0.0 0.0 0.7 1.0) $ GLO.translate x y $ GLO.Circle (realToFrac cellWidth / 2)
      where
        (x, y) = transformToWindow coord
    renderAgent (coord, Infected)    
        = GLO.color (GLO.makeColor 0.7 0.0 0.0 1.0) $ GLO.translate x y $ GLO.ThickCircle 0 (realToFrac cellWidth)
      where
        (x, y) = transformToWindow coord
    renderAgent (coord, Recovered)   
        = GLO.color (GLO.makeColor 0.0 0.70 0.0 1.0) $ GLO.translate x y $ GLO.ThickCircle 0 (realToFrac cellWidth)
      where
        (x, y) = transformToWindow coord

    transformToWindow :: Disc2dCoord -> (Float, Float)
    transformToWindow (x, y) = (x', y')
      where
        rw = cellWidth
        rh = cellHeight

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        x' = fromRational (toRational (fromIntegral x * rw)) - halfXSize
        y' = fromRational (toRational (fromIntegral y * rh)) - halfYSize

mkSimCtx :: RandomGen g
         => SimSF g
         -> SIREnv
         -> g
         -> Integer
         -> Time
         -> SimCtx g
mkSimCtx sf env g steps t = SimCtx {
    simSf    = sf
  , simEnv   = env
  , simRng   = g
  , simSteps = steps
  , simTime  = t
  , simInput = (0, False, False, False)
  }

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
      makeContact <- occasionally (1 / contactRate) () -< ()

      if not $ isEvent makeContact 
        then returnA -< ((), NoEvent)
        else (do
          e <- arrM_ (lift get) -< ()
          let ns = neighbours e coord agentGridSize moore
          --let ns = allNeighbours e
          s <- drawRandomElemS       -< ns
          case s of
            Infected -> do
              infected <- arrM_ (lift $ lift $ randomBoolM infectivity) -< ()
              if infected 
                then (do
                  arrM (put . changeCell coord Infected) -< e
                  returnA -< ((), Event ()))
                else returnA -< ((), NoEvent)
            _       -> returnA -< ((), NoEvent))

infectedAgent :: RandomGen g => Disc2dCoord -> SIRAgent g
infectedAgent coord = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) () ((), Event ())
    infected = proc _ -> do
      recovered <- occasionally illnessDuration () -< ()
      if isEvent recovered
        then (do
          e <- arrM_ (lift get) -< ()
          arrM (put . changeCell coord Recovered) -< e
          returnA -< ((), Event ()))
        else returnA -< ((), NoEvent)

recoveredAgent :: RandomGen g => SIRAgent g
recoveredAgent = returnA 

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
    nCoords  = map (\(x', y') -> (x + x', y + y')) n
    nCoords' = filter (\(nx, ny) -> nx >= 0 && 
                                    ny >= 0 && 
                                    nx <= (dx - 1) &&
                                    ny <= (dy - 1)) nCoords
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