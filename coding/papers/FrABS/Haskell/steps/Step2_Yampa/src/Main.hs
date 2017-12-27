{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Debug.Trace

import Control.Monad.Random
import FRP.Yampa

import SIR

type SIRAgent = SF [SIRState] SIRState

agentCount :: Int
agentCount = 100

infectedCount :: Int
infectedCount = 10

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.1

t :: Time
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  let as = initAgents agentCount infectedCount
  let ass = runSimulation g t dt as

  let dyns = aggregateAllStates ass
  let fileName =  "STEP_2_YAMPA_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulation :: RandomGen g 
              => g 
              -> Time 
              -> DTime 
              -> [SIRState] 
              -> [[SIRState]]
runSimulation g t dt as = embed (stepSimulation 0 sfs as) ((), dts)
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway
    n = length as

    (rngs, _) = rngSplits g n []
    sfs = map (\(g', a) -> sirAgent g' a) (zip rngs as)
    
    rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

stepSimulation :: Int -> [SIRAgent] -> [SIRState] -> SF () [SIRState]
stepSimulation i sfs as =
    pSwitch
      --(\_ sfs' -> trace ("i = " ++ show i) (map (\sf -> (as, sf)) sfs'))
      (\_ sfs' -> (map (\sf -> (as, sf)) sfs'))
      sfs
      (switchingEvt >>> notYet) -- if we switch immediately we end up in endless switching, so always wait for 'next'
      cont

  where
    switchingEvt :: SF ((), [SIRState]) (Event [SIRState])
    switchingEvt = arr (\(_, newAs) -> Event newAs)

    cont :: [SIRAgent] -> [SIRState] -> SF () [SIRState]
    cont sfs' newAs = stepSimulation (i+1) sfs' newAs

sirAgent :: RandomGen g => g -> SIRState -> SIRAgent
sirAgent g Susceptible = susceptibleAgent g
sirAgent g Infected    = infectedAgent g
sirAgent _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => g -> SIRAgent
susceptibleAgent g = 
    switch 
      (susceptible g) 
      (const $ infectedAgent g)
  where
    susceptible :: RandomGen g => g -> SF [SIRState] (SIRState, Event ())
    susceptible g = proc as -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()

      if isEvent makeContact
        then (do
          a <- drawRandomElemSF g         -< as
          i <- randomBoolSF g 0.9 -< () -- TODO: if this 1.0 it works, all /= 1.0 seems to fuck up the whole thing, dunno why

          if (Infected == a && i)
            then returnA -< trace ("a = " ++ show a ++ " i = " ++ show i) (Infected, Event ())
            else returnA -< (Susceptible, NoEvent))
        else returnA -< (Susceptible, NoEvent)

infectedAgent :: RandomGen g => g -> SIRAgent
infectedAgent g = 
    switch 
      infected 
      (const recoveredAgent)
  where
    infected :: SF [SIRState] (SIRState, Event ())
    infected = proc _ -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

recoveredAgent :: SIRAgent
recoveredAgent = arr (const Recovered)

randomBoolSF :: RandomGen g => g -> Double -> SF () Bool
randomBoolSF g p = proc _ -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  --returnA -< trace ("r = " ++ show r ++ " p = " ++ show p ++ " r <= p : " ++ show (r <= p)) (r <= p)
  returnA -< (r <= p)

drawRandomElemSF :: (RandomGen g, Show a) => g -> SF [a] a
drawRandomElemSF g = proc as -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  let len = length as
  let idx = (fromIntegral $ len) * r
  let a =  as !! (floor idx)
  --returnA -< trace ("a = " ++ show a) a
  returnA -< a

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected