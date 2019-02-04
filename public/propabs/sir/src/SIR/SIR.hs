{-# LANGUAGE Arrows #-}
module SIR.SIR 
  ( SIRSimCtx (..)
  , SIRState (..)

  , defaultSIRCtx

  , runSIR
  , runSIRUntil

  -- expose agents behaviour functions as well for testing
  , susceptibleAgent
  , infectedAgent
  , recoveredAgent
  ) where

import Control.Monad.Random
import FRP.Yampa

import SIR.Utils

data SIRSimCtx g = SIRSimCtx
  { syCtxTimeLimit   :: !Double
  , syCtxTimeDelta   :: !Double

  , syCtxRng         :: g

  , syCtxSusceptible :: !Int
  , syCtxInfected    :: !Int
  , syCtxRecovered   :: !Int

  , syCtxContactRate :: !Double
  , syCtxInfectivity :: !Double
  , syCtxIllnessDur  :: !Double
  }

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

type SIRAgent = SF [SIRState] SIRState

runSIR :: RandomGen g 
       => SIRSimCtx g
       -> [(Double, Double, Double)]
runSIR simCtx
    = runSIRUntil g t dt as cr inf dur
  where
    t  = syCtxTimeLimit simCtx
    dt = syCtxTimeDelta simCtx
    g  = syCtxRng simCtx

    s = syCtxSusceptible simCtx
    i = syCtxInfected simCtx
    r = syCtxRecovered simCtx

    cr  = syCtxContactRate simCtx
    inf = syCtxInfectivity simCtx
    dur = syCtxIllnessDur simCtx

    as = initAgents s i r

runSIRUntil :: RandomGen g
            => g
            -> Time
            -> DTime
            -> [SIRState]
            -> Double
            -> Double
            -> Double
            -> [(Double, Double, Double)]
runSIRUntil g0 t dt as0 cr inf0 dur 
    = map sirAggregate ass 
  where
    steps = floor $ t / dt
    dts   = replicate steps (dt, Nothing)

    (rngs, _) = rngSplits g0 (length as0) []
    sfs       = zipWith (sirAgent cr inf0 dur) rngs as0

    ass       = embed (stepSimulation sfs as0) ((), dts)

    sirAggregate :: [SIRState] -> (Double, Double, Double)
    sirAggregate as = (sus, inf, recs)
      where
        sus  = fromIntegral $ length $ filter (==Susceptible) as
        inf  = fromIntegral $ length $ filter (==Infected) as
        recs = fromIntegral $ length $ filter (==Recovered) as

stepSimulation :: [SIRAgent] 
               -> [SIRState] 
               -> SF () [SIRState]
stepSimulation sfs as =
    dpSwitch
      (\_ sfs' -> map (\sf -> (as, sf)) sfs')
      sfs
      -- if we switch immediately we end up in endless switching, so always wait for 'next'
      (switchingEvt >>> notYet) 
      stepSimulation

  where
    switchingEvt :: SF ((), [SIRState]) (Event [SIRState])
    switchingEvt = arr (\(_, newAs) -> Event newAs)

sirAgent :: RandomGen g 
         => Double
         -> Double
         -> Double
         -> g 
         -> SIRState 
         -> SIRAgent
sirAgent cr inf dur g Susceptible = susceptibleAgent cr inf dur g
sirAgent _  _   dur g Infected    = infectedAgent dur g
sirAgent _  _   _   _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g
                 => Double
                 -> Double
                 -> Double
                 -> g
                 -> SIRAgent
susceptibleAgent contactRate infectivity illnessDuration g0 = 
    switch 
      susceptible
      (const $ infectedAgent illnessDuration gInf)
  where
    -- avoid correlation of RNG-streams
    (gOcc, g1)    = split g0
    (gCont, g2)   = split g1
    (gBool, gInf) = split g2

    susceptible :: SF [SIRState] (SIRState, Event ())
    susceptible = proc as -> do
      makeContact <- occasionally gOcc (1 / contactRate) () -< ()
      if isEvent makeContact
        then (do
          a <- drawRandomElemSFSafe gCont -< as
          case a of
            Just Infected -> do
              i <- randomBoolSF gBool -< infectivity
              if i
                then returnA -< (Infected, Event ())
                else returnA -< (Susceptible, NoEvent)
            _       -> returnA -< (Susceptible, NoEvent))
        else returnA -< (Susceptible, NoEvent)

infectedAgent :: RandomGen g 
              => Double
              -> g 
              -> SIRAgent
infectedAgent illnessDuration g = 
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
recoveredAgent = constant Recovered

randomBoolSF :: RandomGen g => g -> SF Double Bool
randomBoolSF g = proc p -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  returnA -< (r <= p)

drawRandomElemSFSafe :: RandomGen g => g -> SF [a] (Maybe a)
drawRandomElemSFSafe g = proc as -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  if null as
    then returnA -< Nothing
    else do

      let len = length as
          idx = fromIntegral len * r
          a   =  as !! floor idx
          
      returnA -< Just a

initAgents :: Int -> Int -> Int -> [SIRState]
initAgents s i r = sus ++ inf ++ recs
  where
    sus = replicate s Susceptible
    inf = replicate i Infected
    recs = replicate r Recovered

defaultSIRCtx :: RandomGen g 
              => g
              -> SIRSimCtx g 
defaultSIRCtx g = SIRSimCtx {
    syCtxTimeLimit   = 150
  , syCtxTimeDelta   = 0.01

  , syCtxRng         = g

  , syCtxSusceptible = 999
  , syCtxInfected    = 1
  , syCtxRecovered   = 0

  , syCtxContactRate = 5
  , syCtxInfectivity = 0.05
  , syCtxIllnessDur  = 15
  }
