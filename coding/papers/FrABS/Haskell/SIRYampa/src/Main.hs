{-# LANGUAGE Arrows #-}
module Main where

import Control.Monad.Random
import System.IO
import Text.Printf

import FRP.Yampa

-- a SIR agent is in one of these states at any time
data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

type SIRAgentProc = SF Agents SIRState

type Agents = [SIRState]

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
dt = 0.1

t :: Time
t = 150.0

main :: IO ()
main = do
  let g = mkStdGen rngSeed
  setStdGen g

  let as = initAgents agentCount infectedCount
  let ass = runSimulationUntil g t dt as

  let dyns = aggregateDynamics ass
  let fileName =  "SIR_YAMPA_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeDynamicsToFile fileName dyns

runSimulationUntil :: (RandomGen g) => g -> Time -> DTime -> Agents -> [Agents]
runSimulationUntil g t dt as = embed (sirSimulation sfs as) ((), dts)
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway
    
    (rngs, _) = rngSplits g steps []
    sfs = map (\(g, a) -> sirAgent g a) (zip rngs as)

    rngSplits :: (RandomGen g) => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g' (n-1) (g'' : acc)
      where
        (g', g'') = split g

sirSimulation :: [SIRAgentProc] -> Agents -> SF () Agents
sirSimulation sfs as = 
    pSwitch 
      (\_ sfs' -> map (\sf -> (as, sf)) sfs') 
      sfs
      (sirSimulationSwitchingEvent >>> notYet) -- if we switch immediately we end up in endless switching, so always wait for 'next'
      sirSimulationContinuation

  where
    sirSimulationSwitchingEvent :: SF ((), Agents) (Event Agents) 
    sirSimulationSwitchingEvent = proc (_, newAs) -> do
      returnA -< (Event newAs)

    sirSimulationContinuation :: [SIRAgentProc] -> Agents -> SF () Agents
    sirSimulationContinuation sfs newAs = sirSimulation sfs newAs

sirAgent :: (RandomGen g) => g -> SIRState -> SIRAgentProc
sirAgent g Susceptible = susceptibleAgent g
sirAgent g Infected = infectedAgent g
sirAgent _ Recovered = recoveredAgent

susceptibleAgent :: (RandomGen g) => g -> SIRAgentProc
susceptibleAgent g = switch (susceptibleAgentInfectedEvent g) (susceptibleAgentInfected g)
  where
    susceptibleAgentInfectedEvent :: (RandomGen g) => g -> SF Agents (SIRState, Event ())
    susceptibleAgentInfectedEvent g = proc as -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()
      a <- drawRandomElemSF g -< as
      doInfect <- randomBoolSF g infectionProb -< ()

      if isEvent makeContact 
        then 
          if Infected == a
            then 
              if doInfect 
                then returnA -< (Infected, Event ()) 
                else returnA -< (Susceptible, NoEvent)
            else returnA -< (Susceptible, NoEvent)
        else
          returnA -< (Susceptible, NoEvent)

    susceptibleAgentInfected :: (RandomGen g) => g -> () -> SIRAgentProc
    susceptibleAgentInfected g _ = infectedAgent g

infectedAgent :: (RandomGen g) => g -> SIRAgentProc
infectedAgent g = switch infectedAgentRecoveredEvent infectedAgentRecovers
  where
    infectedAgentRecoveredEvent :: SF Agents (SIRState, Event ())
    infectedAgentRecoveredEvent = proc _ -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

    infectedAgentRecovers :: () -> SIRAgentProc
    infectedAgentRecovers _ = recoveredAgent

recoveredAgent :: SIRAgentProc
recoveredAgent = proc _ -> do returnA -< Recovered

initAgents :: Int -> Int -> Agents
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

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

drawRandomElemSF :: (RandomGen g) => g -> SF [a] a
drawRandomElemSF g = proc as -> do
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

aggregateDynamics :: [Agents] -> [(Int, Int, Int)]
aggregateDynamics ass = map aggregate ass

aggregate :: Agents -> (Int, Int, Int)
aggregate as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = length $ filter (Susceptible==) as
    infectedCount = length $ filter (Infected==) as
    recoveredCount = length $ filter (Recovered==) as

writeDynamicsToFile :: String -> [(Int, Int, Int)] -> IO ()
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

sirDynamicToString :: (Int, Int, Int) -> String
sirDynamicToString (susceptibleCount, infectedCount, recoveredCount) = 
  printf "%d" susceptibleCount 
  ++ "," ++ printf "%d" infectedCount
  ++ "," ++ printf "%d" recoveredCount
  ++ ";" 