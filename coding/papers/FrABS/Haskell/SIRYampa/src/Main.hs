{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Debug.Trace

import Control.Monad.Random
import FRP.Yampa

import SIR

type SIRAgentProc = SF [SIRState] SIRState

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

dt :: DTime
dt = 0.01

t :: Time
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  setStdGen g

  let as = initAgents agentCount infectedCount
  let ass = runSimulationUntil g t dt as

  let dyns = aggregateAllStates ass
  let fileName =  "SIR_YAMPA_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: (RandomGen g) => g -> Time -> DTime -> [SIRState] -> [[SIRState]]
runSimulationUntil g t dt as = embed (sirSimulation sfs as) ((), dts)
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway

    (rngs, _) = rngSplits g steps []
    sfs = map (\(g', a) -> sirAgent g' a) (zip rngs as)

    rngSplits :: (RandomGen g) => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

sirSimulation :: [SIRAgentProc] -> [SIRState] -> SF () [SIRState]
sirSimulation sfs as =
    pSwitch
      (\_ sfs' -> map (\sf -> (as, sf)) sfs')
      sfs
      (sirSimulationSwitchingEvent >>> notYet) -- if we switch immediately we end up in endless switching, so always wait for 'next'
      sirSimulationContinuation

  where
    sirSimulationSwitchingEvent :: SF ((), [SIRState]) (Event [SIRState])
    sirSimulationSwitchingEvent = proc (_, newAs) -> do returnA -< (Event newAs)

    sirSimulationContinuation :: [SIRAgentProc] -> [SIRState] -> SF () [SIRState]
    sirSimulationContinuation sfs newAs = sirSimulation sfs newAs

sirAgent :: (RandomGen g) => g -> SIRState -> SIRAgentProc
sirAgent g Susceptible = susceptibleAgent g
sirAgent g Infected    = infectedAgent g
sirAgent _ Recovered   = recoveredAgent

susceptibleAgent :: (RandomGen g) => g -> SIRAgentProc
susceptibleAgent g = switch (susceptibleAgentInfectedEvent g) (const $ infectedAgent g)
  where
    susceptibleAgentInfectedEvent :: (RandomGen g) => g -> SF [SIRState] (SIRState, Event ())
    susceptibleAgentInfectedEvent g = proc as -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()
      a <- drawRandomElemSF g -< as
      doInfect <- randomBoolSF g infectionProb -< ()

      --if (trace ("makeContact = " ++ show makeContact ++ ", a = " ++ show a ++ ", doInfect = " ++ show doInfect) (isEvent makeContact))
      --if (trace ("as = " ++ show as) (isEvent makeContact))
      if isEvent makeContact
          && Infected == a
          && doInfect
        then returnA -< (trace ("Infected") (Infected, Event ()))
        else returnA -< (trace ("Susceptible") (Susceptible, NoEvent))

infectedAgent :: (RandomGen g) => g -> SIRAgentProc
infectedAgent g = switch infectedAgentRecoveredEvent (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: SF [SIRState] (SIRState, Event ())
    infectedAgentRecoveredEvent = proc _ -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

recoveredAgent :: SIRAgentProc
recoveredAgent = arr (const Recovered)

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