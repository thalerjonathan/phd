{-# LANGUAGE Arrows #-}
module ABSFeedback
  (
    runABS

  , susceptibleAgent
  , infectedAgent
  ) where

import Control.Monad.Random
import FRP.Yampa

import SIR

type SIRAgent = SF [SIRState] SIRState

runABS :: RandomGen g 
       => g 
       -> Int
       -> Int
       -> Double
       -> Double
       -> Double 
       -> Time 
       -> DTime 
       -> [(Double, Double, Double)]
runABS g populationSize infectedCount contactRate infectivity illnessDuration t dt
    = aggregateAllStates $ runSimulation g contactRate infectivity illnessDuration t dt as
  where
    as = initAgents populationSize infectedCount
    
runSimulation :: RandomGen g 
              => g 
              -> Double
              -> Double
              -> Double 
              -> Time 
              -> DTime 
              -> [SIRState] 
              -> [[SIRState]]
runSimulation g contactRate infectivity illnessDuration t dt as 
    = embed (stepSimulation sfs as) ((), dts)
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway
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

    sirAgent :: RandomGen g 
            => g 
            -> SIRState
            -> SIRAgent
    sirAgent g Susceptible 
        = susceptibleAgent g0 g1 g2 g3 contactRate infectivity illnessDuration
      where
        (g', g0) = split g
        (g'', g1) = split g'
        (g2, g3) = split g''

    sirAgent g Infected    
      = infectedAgent g illnessDuration
    sirAgent _ Recovered 
      = recoveredAgent

susceptibleAgent :: RandomGen g 
                 => g
                 -> g
                 -> g
                 -> g
                 -> Double
                 -> Double
                 -> Double 
                 -> SIRAgent
susceptibleAgent g0 g1 g2 g3 contactRate infectivity illnessDuration = 
    switch
      (susceptible g0 g1 g2) 
      (const $ infectedAgent g3 illnessDuration)
  where
    susceptible :: RandomGen g => g -> g -> g -> SF [SIRState] (SIRState, Event ())
    susceptible g0 g1 g2 = proc as -> do
      makeContact <- occasionally g0 (1 / contactRate) () -< ()

      -- NOTE: strangely if we are not splitting all if-then-else into
      -- separate but only a single one, then it seems not to work,
      -- dunno why
      if isEvent makeContact
        then (do
          a <- drawRandomElemSF g1 -< as
          case a of
            Just Infected -> do
              i <- randomBoolSF g2 infectivity -< ()
              if i
                then returnA -< (Infected, Event ())
                else returnA -< (Susceptible, NoEvent)
            _       -> returnA -< (Susceptible, NoEvent))
        else returnA -< (Susceptible, NoEvent)

infectedAgent :: RandomGen g 
              => g 
              -> Double
              -> SIRAgent
infectedAgent g illnessDuration = 
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

drawRandomElemSF :: RandomGen g => g -> SF [a] (Maybe a)
drawRandomElemSF g = proc as -> 
  if null as 
    then returnA -< Nothing
    else do
      r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
      let len = length as
      let idx = fromIntegral len * r
      let a =  as !! floor idx
      returnA -< Just a

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected