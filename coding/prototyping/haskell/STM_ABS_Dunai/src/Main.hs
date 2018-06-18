{-# LANGUAGE Arrows     #-}
module Main where

import System.IO
import Text.Printf

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Random
import Data.Array.IArray
import FRP.BearRiver
import qualified Graphics.Gloss as GLO
import qualified Graphics.Gloss.Interface.IO.Game as GLOGame

import Control.Concurrent.Async
import Control.Concurrent.STM

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

type Disc2dCoord  = (Int, Int)
type SIREnv       = Array Disc2dCoord SIRState
type SIRMonad g   = RandT g STM --StateT SIREnv (Rand g)
type SIRAgent g   = SF (SIRMonad g) () ()

agentGridSize :: (Int, Int)
agentGridSize = (21, 21)

rngSeed :: Int
rngSeed = 123

dt :: DTime
dt = 0.1

t :: Time
t = 10

winSize :: (Int, Int)
winSize = (600, 600)

winTitle :: String
winTitle = "Concurrent (STM) Agent-Based SIR on 2D Grid"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g         = mkStdGen rngSeed
      (as, e)   = initAgentsEnv agentGridSize

  es <- runSimulation g t dt e as

  let ass       = environmentsToAgentDyns es
      dyns      = aggregateAllStates ass
      fileName  =  "STMABSDunai_DYNAMICS_" ++ show agentGridSize ++ "agents.m"
  
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
              -> IO [SIREnv]
runSimulation g t dt e as = do
  -- TODO: replace by TArray, otherwise every access to the environment will result in a retry of the transaction
  env <- newTVarIO e

  let steps = floor $ t / dt
      dts = replicate steps ()
      sfs = map (uncurry $ sirAgent env) as

      esReader = embed (stepSimulation sfs env) dts
      esRand = runReaderT esReader dt
  
  evalRandT esRand g

-- type SIRMonad g   = RandT g STM --StateT SIREnv (Rand g)
-- type SIRAgent g   = SF (SIRMonad g) () ()

stepSimulation :: RandomGen g
               => [SIRAgent g]
               -> TVar SIREnv 
               -> SF (RandT g IO) () SIREnv
stepSimulation sfs env = MSF $ \_ -> do
  -- TODO: this doesnt work like this, need to lift into IO to get rid of STM
  ret <- mapM (\sf -> async $ atomically (unMSF () sf)) sfs
  _

  -- NOTE: waiting for agents to finish
  res <- mapM wait ret

  let sfs'  = fmap snd res
  e <- readTVarIO env
  let cont = stepSimulation sfs' env
  return (e, cont)

sirAgent :: RandomGen g 
         => TVar SIREnv
         -> Disc2dCoord 
         -> SIRState 
         -> SIRAgent g
sirAgent env c Susceptible = susceptibleAgent env c 
sirAgent env c Infected    = infectedAgent env c 
sirAgent _   _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g 
                 => TVar SIREnv 
                 -> Disc2dCoord
                 -> SIRAgent g
susceptibleAgent env coord = 
    switch 
      susceptible
      (const $ infectedAgent env coord)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) () ((), Event ())
    susceptible = proc _ -> do
      makeContact <- occasionally (1 / contactRate) () -< ()

      if not $ isEvent makeContact 
        then returnA -< ((), NoEvent)
        else (do
          --e <- arrM_ (lift get) -< ()
          e <- arrM_ (lift $ lift $ readTVar env) -< ()
          let ns = neighbours e coord agentGridSize moore
          --let ns = allNeighbours e
          s <- drawRandomElemS       -< ns
          case s of
            Infected -> do
              infected <- arrM_ (lift $ randomBool infectivity) -< ()
              if infected 
                then (do
                  --arrM (put . changeCell coord Infected) -< e
                  let e' = changeCell coord Infected e
                  arrM (lift . lift . writeTVar env) -< e'
                  returnA -< ((), Event ()))
                else returnA -< ((), NoEvent)
            _       -> returnA -< ((), NoEvent))

infectedAgent :: RandomGen g 
              => TVar SIREnv 
              -> Disc2dCoord
              -> SIRAgent g
infectedAgent env coord = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) () ((), Event ())
    infected = proc _ -> do
      recovered <- occasionally illnessDuration () -< ()
      if isEvent recovered
        then (do
          --e <- arrM_ (lift get) -< ()
          --e <- arrM_ (lift $ lift $ readTVar env) -< ()
          -- arrM (put . changeCell coord Recovered) -< e
          arrM_ (lift $ lift $ modifyTVar env (changeCell coord Recovered)) -< ()
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

randomBool :: MonadRandom m => Double -> m Bool
randomBool p = getRandomR (0, 1) >>= (\r -> return $ r <= p)


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

aggregateAllStates :: [[SIRState]] -> [(Double, Double, Double)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Double, Double, Double)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter (Susceptible==) as
    infectedCount = fromIntegral $ length $ filter (Infected==) as
    recoveredCount = fromIntegral $ length $ filter (Recovered==) as

writeAggregatesToFile :: String -> [(Double, Double, Double)] -> IO ()
writeAggregatesToFile fileName dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirAggregateToString) dynamics
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "susceptible = dynamics (:, 1);"
  hPutStrLn fileHdl "infected = dynamics (:, 2);"
  hPutStrLn fileHdl "recovered = dynamics (:, 3);"
  hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  hPutStrLn fileHdl "steps = length (susceptible);"
  hPutStrLn fileHdl "indices = 0 : steps - 1;"

  hPutStrLn fileHdl "figure"
  hPutStrLn fileHdl "plot (indices, susceptibleRatio.', 'color', 'blue', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (indices, infectedRatio.', 'color', 'red', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (indices, recoveredRatio.', 'color', 'green', 'linewidth', 2);"

  hPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0);"
  
  hPutStrLn fileHdl "xlabel ('Time');"
  hPutStrLn fileHdl "ylabel ('Population Ratio');"
  hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

  hClose fileHdl

sirAggregateToString :: (Double, Double, Double) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%f" susceptibleCount
  ++ "," ++ printf "%f" infectedCount
  ++ "," ++ printf "%f" recoveredCount
  ++ ";"