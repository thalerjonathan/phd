{-# LANGUAGE Arrows     #-}
module Main where

import System.IO

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Random
import qualified Data.Map as Map
import Data.Array.IArray
import FRP.BearRiver

import SIR

data AgentIn = AgentIn
  {
    aiId    :: !AgentId
  } deriving (Show)

data AgentOut o = AgentOut
  {
    aoObservable  :: !o
  } deriving (Show)

type Agent m o    = SF m (AgentIn) (AgentOut o)

type SIREnv       = Array Discrete2dCoord SIRState
type SIRMonad g   = StateT SIREnv (Rand g)
type SIRAgentIn   = AgentIn
type SIRAgentOut  = AgentOut SIRState
type SIRAgent g   = Agent (SIRMonad g) SIRState

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
  let fileName =  "STEP_4_BEARRIVER_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

initAgents :: Int -> Int -> [(AgentId, SIRState)]
initAgents n i = sus ++ inf
  where
    sus = map (\ai -> (ai, Susceptible)) [0..n-i-1]
    inf = map (\ai -> (ai, Infected)) [n-i..n-1]

runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> SIREnv
              -> [(AgentId, SIRState)] 
              -> [[SIRState]]
runSimulation g t dt e as = map (\aos -> map aoObservable aos) aoss
  where
    steps = floor $ t / dt
    dts = replicate steps ()

    ais = map fst as
    sfs = map (\(_, s) -> sirAgent ais s) as
    ains = map (\(aid, _) -> agentIn aid) as

    aossReader = embed (stepSimulation sfs ains) dts
    aossRand = runReaderT aossReader dt
    aoss = evalRand aossRand g

stepSimulation :: RandomGen g
               => [SIRAgent g] 
               -> [SIRAgentIn] 
               -> SF (SIRMonad g) () [SIRAgentOut]
stepSimulation sfs ains =
    dpSwitch
      (\_ sfs' -> (zip ains sfs'))
      sfs
      switchingEvt -- no need for 'notYet' in BearRiver as there is no time = 0 with dt = 0
      cont

  where
    switchingEvt :: RandomGen g
                 => SF (SIRMonad g) ((), [SIRAgentOut]) (Event [SIRAgentIn])
    switchingEvt = proc (_, aos) -> do
      let ais      = map aiId ains
          aios     = zip ais aos
          nextAins = distributeData aios
      returnA -< Event nextAins

    cont :: RandomGen g 
         => [SIRAgent g] 
         -> [SIRAgentIn] 
         -> SF (SIRMonad g) () [SIRAgentOut]
    cont sfs nextAins = stepSimulation sfs nextAins

sirAgent :: RandomGen g => [AgentId] -> SIRState -> SIRAgent g
sirAgent ais Susceptible = susceptibleAgent ais
sirAgent _   Infected    = infectedAgent
sirAgent _   Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => [AgentId] -> SIRAgent g
susceptibleAgent ais = 
    switch 
      susceptible
      (const $ infectedAgent)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) SIRAgentIn (SIRAgentOut, Event ())
    susceptible = proc ain -> do
      infected <- arrM (\ain -> lift $ gotInfected infectivity ain) -< ain

      if infected 
        then returnA -< (agentOut Infected, Event ())
        else (do
          makeContact <- occasionallyM (1 / contactRate) () -< ()
          contactId   <- drawRandomElemS                    -< ais

          if isEvent makeContact
            then returnA -< (dataFlow (contactId, Contact Susceptible) $ agentOut Susceptible, NoEvent)
            else returnA -< (agentOut Susceptible, NoEvent))

infectedAgent :: RandomGen g => SIRAgent g
infectedAgent = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) SIRAgentIn (SIRAgentOut, Event ())
    infected = proc ain -> do
      recEvt <- occasionallyM illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      -- note that at the moment of recovery the agent can still infect others
      -- because it will still reply with Infected
      let ao = respondToContactWith Infected ain (agentOut a)
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

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId    = aid
  }

agentOut :: o -> AgentOut o d
agentOut o = AgentOut {
    aoObservable  = o
  }