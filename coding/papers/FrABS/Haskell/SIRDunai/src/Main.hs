{-# LANGUAGE Arrows #-}
module Main where

import Control.Monad.Random
import System.IO
import Text.Printf
import Debug.Trace

import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction

-- a SIR agent is in one of these states at any time
data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

type SIRAgentMSF g = RandomGen g => MSF (Rand g) Double SIRState

contactRate :: Double
contactRate = 5.0

infectionProb :: Double
infectionProb = 0.05

illnessDuration :: Double
illnessDuration = 15.0

agentCount :: Int
agentCount = 100

infectedCount :: Int
infectedCount = 10

rngSeed :: Int
rngSeed = 42

dt :: Double
dt = 0.01

t :: Double
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  
  let as = initAgents agentCount infectedCount
  let ass = runSimulationUntil g t dt as

  let dyns = aggregateDynamics ass
  let fileName =  "SIR_YAMPA_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeDynamicsToFile fileName dyns

runSimulationUntil :: (RandomGen g) => g -> Double -> Double -> [SIRState] -> [[SIRState]]
runSimulationUntil g t dt as = evalRand ass g
  where
    steps = floor $ t / dt
    dts = replicate steps dt
    msfs = map sirAgent as

    ass = embed (sirSimulation msfs as) dts

sirSimulation :: [SIRAgentMSF] -> [SIRState] -> MSF Rand Double [SIRState]
sirSimulation sfs as = undefined

sirAgent :: SIRState -> SIRAgentMSF
sirAgent g Susceptible = susceptibleAgent g
sirAgent g Infected    = infectedAgent g
sirAgent _ Recovered   = recoveredAgent


-- Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b

susceptibleAgent :: SIRAgentMSF
susceptibleAgent = switch susceptibleAgentInfectedEvent (const infectedAgent)
  where
    susceptibleAgentInfectedEvent :: MSF Rand Double (SIRState, Maybe ())
    susceptibleAgentInfectedEvent = proc dt -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()
      a <- drawRandomElemSF g -< as
      doInfect <- randomBoolSF g infectionProb -< ()

      --if (trace ("makeContact = " ++ show makeContact ++ ", a = " ++ show a ++ ", doInfect = " ++ show doInfect) (isEvent makeContact))
      --if (trace ("as = " ++ show as) (isEvent makeContact))
      if isEvent makeContact
          && Infected == a
          && doInfect
        then returnA -< (Infected, Just ())
        else returnA -< (Susceptible, Nothing)

infectedAgent :: SIRAgentMSF
infectedAgent = switch infectedAgentRecoveredEvent (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: MSF Rand Double (SIRState, Maybe ())
    infectedAgentRecoveredEvent = proc dt -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = maybe Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

recoveredAgent :: SIRAgentMSF
recoveredAgent = arr (const Recovered)

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

{-
randomBoolSF :: (RandomGen g) => g -> Double -> SF () Bool
randomBoolSF g p = proc _ -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  returnA -< (r <= p)

drawRandomElemSF :: (RandomGen g, Show a) => g -> SF [a] a
drawRandomElemSF g = proc as -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  let len = length as
  let idx = (fromIntegral $ len) * r
  let a =  as !! (floor idx)
  returnA -< a
-}

aggregateDynamics :: [[SIRState]] -> [(Int, Int, Int)]
aggregateDynamics ass = map aggregate ass

aggregate :: [SIRState] -> (Int, Int, Int)
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
