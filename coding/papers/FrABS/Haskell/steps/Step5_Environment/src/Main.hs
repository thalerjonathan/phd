{-# LANGUAGE Arrows     #-}
module Main where

import System.IO

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.MSF.Random
import Data.Array.IArray
import FRP.BearRiver

import SIR

data AgentOut o = AgentOut
  {
    aoObservable  :: !o
  } deriving (Show)

type Agent m o    = SF m () (AgentOut o)

type Disc2dCoord  = (Int, Int)
type SIREnv       = Array Disc2dCoord SIRState
type SIRMonad g   = StateT SIREnv (Rand g)
type SIRAgentOut  = AgentOut SIRState
type SIRAgent g   = Agent (SIRMonad g) SIRState

agentGrid :: (Int, Int)
agentGrid = (10, 10)

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.1

t :: Time
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g   = mkStdGen rngSeed
      as  = initAgents agentGrid
      e   = initEnvironment agentGrid as
      ass = runSimulation g t dt e as

      dyns      = aggregateAllStates ass
      fileName  =  "STEP_5_ENVIRONMENT_DYNAMICS_" ++ show agentGrid ++ "agents.m"
  writeAggregatesToFile fileName dyns

initAgents :: (Int, Int) -> [(Disc2dCoord, SIRState)]
initAgents (xd, yd) = inf : sus
  where
    xCenter = floor $ (fromIntegral xd) * (0.5 :: Double)
    yCenter = floor $ (fromIntegral xd) * (0.5 :: Double)
    
    sus = [ ((x, y), Susceptible) | x <- [0..xd-1], 
                                    y <- [0..yd-1], 
                                    x /= xCenter && 
                                    y /= yCenter ] 
    inf = ((xCenter, yCenter), Infected)

initEnvironment :: (Int, Int) -> [(Disc2dCoord, SIRState)] -> SIREnv
initEnvironment (xd, yd) as = array ((0, 0), (xd - 1, yd - 1)) as
 
runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> SIREnv
              -> [(Disc2dCoord, SIRState)] 
              -> [[SIRState]]
runSimulation g t dt e as = map (\aos -> map aoObservable aos) aoss
  where
    steps = floor $ t / dt
    dts = replicate steps ()
    sfs = map (\(c, s) -> sirAgent c s) as

    aossReader = embed (stepSimulation sfs) dts
    aossState = runReaderT aossReader dt
    aossRand = evalStateT aossState e
    aoss = evalRand aossRand g

stepSimulation :: RandomGen g
               => [SIRAgent g]
               -> SF (SIRMonad g) () [SIRAgentOut]
stepSimulation sfs =
    dpSwitchB
      sfs
      switchingEvt -- no need for 'notYet' in BearRiver as there is no time = 0 with dt = 0
      cont

  where
    switchingEvt :: RandomGen g
                 => SF (SIRMonad g) ((), [SIRAgentOut]) (Event ())
    switchingEvt = proc _ -> do
      returnA -< Event ()

    cont :: RandomGen g 
         => [SIRAgent g] 
         -> ()
         -> SF (SIRMonad g) () [SIRAgentOut]
    cont sfs _ = stepSimulation sfs

sirAgent :: RandomGen g => Disc2dCoord -> SIRState -> SIRAgent g
sirAgent c Susceptible = susceptibleAgent c
sirAgent _ Infected    = infectedAgent
sirAgent _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => Disc2dCoord -> SIRAgent g
susceptibleAgent _ = 
    switch 
      susceptible
      (const $ infectedAgent)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) () (SIRAgentOut, Event ())
    susceptible = proc _ -> do
      --infected <- arrM (\ain -> lift $ gotInfected infectivity ain) -< ain
      let infected = False

      if infected 
        then returnA -< (agentOut Infected, Event ())
        else (do
          makeContact <- occasionallyM (1 / contactRate) () -< ()
          _contactId   <- drawRandomElemS                    -< []

          if isEvent makeContact
            then returnA -< (agentOut Susceptible, NoEvent)
            else returnA -< (agentOut Susceptible, NoEvent))

infectedAgent :: RandomGen g => SIRAgent g
infectedAgent = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) () (SIRAgentOut, Event ())
    infected = proc _ -> do
      recEvt <- occasionallyM illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      let ao = (agentOut a)
      returnA -< (ao, recEvt)

recoveredAgent :: RandomGen g => SIRAgent g
recoveredAgent = arr (const $ agentOut Recovered)

drawRandomElemS :: MonadRandom m => SF m [a] a
drawRandomElemS = proc as -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  let len = length as
  let idx = (fromIntegral $ len) * r
  let a =  as !! (floor idx)
  returnA -< a

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

agentOut :: o -> AgentOut o
agentOut o = AgentOut {
    aoObservable  = o
  }