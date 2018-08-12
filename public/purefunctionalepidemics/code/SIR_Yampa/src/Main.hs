{-# LANGUAGE Arrows #-}
module Main where

import System.IO

import Control.Monad.Random
import FRP.Yampa

import SIR

type SIRAgent = SF [SIRState] SIRState

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 1

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.01 -- 0.0025

t :: Time
t = 150

sirTest :: SIRAgent
sirTest = dSwitch  
            susceptible
            (const infected)
  where
    susceptible :: SF [SIRState] (SIRState, Event ())
    susceptible = proc _ -> returnA -< (Infected, Event ())

    infected :: SIRAgent
    infected = switch 
            infectedAux
            (const recovered)
      where
        infectedAux :: SF [SIRState] (SIRState, Event ())
        infectedAux = proc _ -> returnA -< (Recovered, Event ())

    recovered :: SIRAgent
    recovered = arr (const Recovered)

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
runSimulation g t dt as = embed (stepSimulation sfs as) ((), dts)
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing)
    n = length as

    (rngs, _) = rngSplits g n []
    sfs = zipWith sirAgent rngs as
    
    rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

stepSimulation :: [SIRAgent] -> [SIRState] -> SF () [SIRState]
stepSimulation sfs as =
    pSwitch
      (\_ sfs' -> (map (\sf -> (as, sf)) sfs'))
      sfs
      -- if we switch immediately we end up in endless switching, so always wait for 'next'
      (switchingEvt >>> notYet) 
      stepSimulation

  where
    switchingEvt :: SF ((), [SIRState]) (Event [SIRState])
    switchingEvt = arr (\(_, newAs) -> Event newAs)

sirAgent :: RandomGen g => g -> SIRState -> SIRAgent
sirAgent g Susceptible = susceptibleAgent g
sirAgent g Infected    = infectedAgent g
sirAgent _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => g -> SIRAgent
susceptibleAgent g = 
    -- NOTE: we need to use a delayed (d)Switch here because
    -- according to the SIR model only a single state-transition 
    -- should occur during one step. If we are not using a delay
    -- we could go from susceptible directly to recovered 
    -- if infected immediately generates a receovery event -
    -- which probability is not very high but still possible
    -- NOTE: we tested it with delay and without, it has no influence
    -- it seems that the probability is way too low for it to happen?
    dSwitch 
      (susceptible g) 
      (const $ infectedAgent g)
  where
    susceptible :: RandomGen g => g -> SF [SIRState] (SIRState, Event ())
    susceptible g = proc as -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()

      -- NOTE: strangely if we are not splitting all if-then-else into
      -- separate but only a single one, then it seems not to work,
      -- dunno why
      if isEvent makeContact
        then (do
          a <- drawRandomElemSF g -< as
          case a of
            Infected -> do
              i <- randomBoolSF g infectivity -< ()
              if i
                then returnA -< (Infected, Event ())
                else returnA -< (Susceptible, NoEvent)
            _       -> returnA -< (Susceptible, NoEvent))
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
  returnA -< (r <= p)

drawRandomElemSF :: RandomGen g => g -> SF [a] a
drawRandomElemSF g = proc as -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  let len = length as
  let idx = fromIntegral len * r
  let a =  as !! floor idx
  returnA -< a

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected