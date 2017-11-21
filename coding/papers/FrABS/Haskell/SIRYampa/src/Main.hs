{-# LANGUAGE Arrows #-}
module Main where

import Control.Monad.Random
import Data.List
import Data.Maybe
import System.IO
import Text.Printf

import FRP.Yampa

-- a SIR agent is in one of these states at any time
data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

type SIRAgent = (SIRState, Time)
type SIRAgentProcIn = (SIRAgent, Agents)
type SIRAgentProcOut = SIRAgent
type SIRAgentProc = SF SIRAgentProcIn SIRAgentProcOut

type Agents = [(SIRState, Time)]

contactRate :: Double
contactRate = 5.0

infectionProb :: Double
infectionProb = 0.05

illnessDuration :: Double
illnessDuration = 15.0

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 10

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 1.0

t :: Time
t = 150.0

susceptible :: SIRAgent
susceptible = (Susceptible, 0)

infected :: Time -> SIRAgent
infected t = (Infected, t)

recovered :: SIRAgent
recovered = (Recovered, 0)

main :: IO ()
main = do
  let g = mkStdGen rngSeed
  setStdGen g

  as <- evalRandIO $ initAgents agentCount infectedCount
  let ass = runSimulationUntil g t dt as

  let a = head $ last ass
  print a 

  let dyns = aggregateDynamics ass
  let fileName =  "SIR_YAMPA_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeDynamicsToFile fileName dyns

runSimulationUntil :: (RandomGen g) => g -> Time -> DTime -> Agents -> [Agents]
runSimulationUntil g t dt as = embed (sirSimulation sfs as) (as, dts)
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway
    
    (rngs, _) = rngSplits g steps []
    sfs = map sirAgent rngs

    rngSplits :: (RandomGen g) => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g' (n-1) (g'' : acc)
      where
        (g', g'') = split g

sirSimulation :: [SIRAgentProc] -> Agents -> SF Agents Agents
sirSimulation sfs as = proc _ -> do -- ignoring input
    pSwitch 
      sirSimulationRoute
      sfs
      (sirSimulationSwitchingEvent >>> notYet) -- if we switch immediately we end up in endless switching, so always wait for 'next'
      sirSimulationContinuation -< as

  where
    sirSimulationRoute :: Agents -> [sf] -> [(SIRAgentProcIn, sf)]
    sirSimulationRoute as sfs = zipWith (\a sf -> ((a, as), sf)) as sfs
    
    sirSimulationSwitchingEvent :: SF (Agents, Agents) (Event Agents) 
    sirSimulationSwitchingEvent = proc (_oldAs, newAs) -> do
      returnA -< (Event newAs)

    sirSimulationContinuation :: [SIRAgentProc] -> Agents -> SF Agents Agents
    sirSimulationContinuation sfs newAs = sirSimulation sfs newAs

sirAgent :: (RandomGen g) => g -> SIRAgentProc
sirAgent g = switch sirAgentStateSwitch id
  where
    sirAgentStateSwitch :: SF SIRAgentProcIn (SIRAgent, Event SIRAgentProc)
    sirAgentStateSwitch = proc (a, _) -> do
      if is Susceptible a
        then returnA -< (a, Event $ susceptibleAgent g)
        else if is Infected a 
          then returnA -< (a, Event infectedAgent)
          else returnA -< (a, Event recoveredAgent)

susceptibleAgent :: (RandomGen g) => g -> SIRAgentProc
susceptibleAgent g = proc ain@(a, as) -> do
    randContCount <- randomExpSF g (1 / contactRate) -< ()
    aInfs <- par 
              (\ain' sfs -> map (\sf -> (ain', sf)) sfs)
              (replicate (floor contactRate) (susceptibleAgentContactSF g)) -< ain
              --(replicate (floor randContCount) (susceptibleAgentContactSF g)) -< ain

    let mayInf = find (is Infected) aInfs
    returnA -< fromMaybe susceptible mayInf

  where
    susceptibleAgentContactSF :: (RandomGen g) => g -> SIRAgentProc
    susceptibleAgentContactSF g = proc (a, as) -> do
      randCont <- drawRandomAgentSF g -< as
      if (is Infected randCont) 
        then infectSF g -< a
        else returnA -< a
     
    infectSF :: (RandomGen g) => g -> SF SIRAgent SIRAgent
    infectSF g = proc a -> do
      doInfect <- randomBoolSF g infectionProb -< ()
      t <- time -< ()
      randIllDur <- randomExpSF g (1 / illnessDuration) -< ()
      if doInfect
        then returnA -< infected (t + randIllDur)
        else returnA -< a

infectedAgent :: SIRAgentProc
infectedAgent = switch infectedAgentRecoveredEvent infectedAgentRecovers
  where
    infectedAgentRecoveredEvent :: SF SIRAgentProcIn (SIRAgent, Event ())
    infectedAgentRecoveredEvent = proc (a@(_, st), _) -> do
      t <- time -< ()
      recEvt <- edge -< (t >= st)
      let a' = event a (\_ -> recovered) recEvt
      returnA -< (a', recEvt)

    infectedAgentRecovers :: () -> SIRAgentProc
    infectedAgentRecovers _ = recoveredAgent

recoveredAgent :: SIRAgentProc
recoveredAgent = arr fst

is :: SIRState -> SIRAgent -> Bool
is s (s',_) = s == s'

initAgents :: (RandomGen g) => Int -> Int -> Rand g Agents
initAgents n i = do
  let sus = replicate (n - i) susceptible
  expTs <- mapM randomExpM (replicate i (1 / illnessDuration))
  let inf = map infected expTs
  return $ sus ++ inf

randomBoolSF :: (RandomGen g) => g -> Double -> SF () Bool
randomBoolSF g p = proc _ -> do
  r <- noiseR (0, 1) g -< ()
  returnA -< (r <= p)

randomExpSF :: (RandomGen g) => g -> Double -> SF () Double
randomExpSF g lambda = proc _ -> do
  r <- noise g -< ()
  if (r == 0)
    then randomExpSF (fst $ split g) lambda -< ()
    else returnA -< ((-log r) / lambda)

drawRandomAgentSF :: (RandomGen g) => g -> SF Agents SIRAgent
drawRandomAgentSF g = proc as -> do
    r <- noiseR (0, 1) g -< ()

    let len = length as
    let idx = (len - 1) * r
    
    returnA -< as !! idx

randomExpM :: (RandomGen g) => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))
  where
    avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r

aggregateDynamics :: [Agents] -> [(Double, Double, Double)]
aggregateDynamics ass = map aggregate ass

aggregate :: Agents -> (Double, Double, Double)
aggregate as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . fst) as
    infectedCount = fromIntegral $ length $ filter ((Infected==) . fst) as
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . fst) as

writeDynamicsToFile :: String -> [(Double, Double, Double)] -> IO ()
writeDynamicsToFile fileName dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirDynamicToString) dynamics
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

sirDynamicToString :: (Double, Double, Double) -> String
sirDynamicToString (susceptibleRatio, infectedRatio, recoveredRatio) = 
  printf "%.3f" susceptibleRatio 
  ++ "," ++ printf "%.3f" infectedRatio
  ++ "," ++ printf "%.3f" recoveredRatio
  ++ ";" 