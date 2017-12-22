{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Debug.Trace

import Control.Monad.Random
import qualified Data.Map as Map
import FRP.Yampa

import SIR

type AgentId     = Int
type AgentData d = (AgentId, d)

data AgentIn d = AgentIn
  {
    aiId    :: !AgentId
  , aiData  :: ![AgentData d]
  } deriving (Show)

data AgentOut o d = AgentOut
  {
    aoData        :: ![AgentData d]
  , aoObservable  :: !o
  } deriving (Show)

type Agent o d    = SF (AgentIn d) (AgentOut o d)

data SIRMsg       = Contact SIRState deriving (Show, Eq)
type SIRAgentIn   = AgentIn SIRMsg
type SIRAgentOut  = AgentOut SIRState SIRMsg
type SIRAgent     = SF SIRAgentIn SIRAgentOut

agentCount :: Int
agentCount = 100

infectedCount :: Int
infectedCount = 50

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

runSimulation :: (RandomGen g) 
              => g 
              -> Time 
              -> DTime 
              -> [SIRState] 
              -> [[SIRState]]
runSimulation g t dt as = map (\aos -> map aoObservable aos) aoss
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway
    n = length as

    (rngs, _) = rngSplits g n []
    sfs = map (\(g', a) -> sirAgent g' a) (zip rngs as)
    ais = []
    aoss = embed (stepSimulation sfs ais) ((), dts)

    rngSplits :: (RandomGen g) => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

stepSimulation :: [SIRAgent] -> [SIRAgentIn] -> SF () [SIRAgentOut]
stepSimulation sfs ais =
    pSwitch
      (\_ sfs' -> (zip ais sfs'))
      sfs
      (switchingEvt >>> notYet) -- if we switch immediately we end up in endless switching, so always wait for 'next'
      cont

  where
    switchingEvt :: SF ((), [SIRAgentOut]) (Event [SIRAgentIn])
    switchingEvt = proc (_, _aos) -> do
      let nextAis = []
      returnA -< Event nextAis

    cont :: [SIRAgent] -> [SIRAgentIn] -> SF () [SIRAgentOut]
    cont sfs nextAis = stepSimulation sfs nextAis

sirAgent :: (RandomGen g) => g -> SIRState -> SIRAgent
sirAgent g Susceptible = trace ("sirAgent: I'm Susceptible") susceptibleAgent g
sirAgent g Infected    = trace ("sirAgent:I'm Infected") infectedAgent g
sirAgent _ Recovered   = trace ("sirAgent:I'm Recovered") recoveredAgent

susceptibleAgent :: (RandomGen g) => g -> SIRAgent
susceptibleAgent g = 
    switch 
      (infectedEvent g) 
      (const $ infectedAgent g)
  where
    infectedEvent :: (RandomGen g) 
                  => g 
                  -> SF SIRAgentIn (SIRAgentOut, Event ())
    infectedEvent _g = proc _as -> do
      --makeContact <- occasionally g (1 / contactRate) () -< ()
      --a <- drawRandomElemSF g -< as
      --doInfect <- randomBoolSF g infectivity -< ()

      returnA -< (agentOut Infected, Event ())

      {-
      if (trace ("makeContact = " ++ show makeContact ++ ", a = " ++ show a ++ ", doInfect = " ++ show doInfect) (isEvent makeContact))
      if (trace ("as = " ++ show as) (isEvent makeContact))
      if isEvent makeContact
          && Infected == a
          && doInfect
        then returnA -< (trace ("Infected") (Infected, Event ()))
        else returnA -< (trace ("Susceptible") (Susceptible, NoEvent))
        -}

infectedAgent :: (RandomGen g) => g -> SIRAgent
infectedAgent g = 
    switch
      infectedAgentRecoveredEvent 
      (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: SF SIRAgentIn (SIRAgentOut, Event ())
    infectedAgentRecoveredEvent = proc _ -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      returnA -< trace ("infectedAgent") (agentOut a, recEvt)

recoveredAgent :: SIRAgent
recoveredAgent = trace ("recoveredAgent") (arr (const $ agentOut Recovered))

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
  --returnA -< trace ("a = " ++ show a) a
  returnA -< a

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

dataFlow :: AgentData d -> AgentOut o d -> AgentOut o d
dataFlow df ao = ao { aoData = df : aoData ao }

onData :: (AgentData d -> acc -> acc) -> AgentIn d -> acc -> acc
onData dHdl ai a 
    | null ds = a
    | otherwise = foldr (\msg acc'-> dHdl msg acc') a ds
  where
    ds = aiData ai

onDataM :: (Monad m) => (acc -> AgentData d -> m acc) -> AgentIn d -> acc -> m acc
onDataM dHdl ai acc
    | null ds = return acc
    | otherwise = foldM dHdl acc ds
  where
    ds = aiData ai

gotInfected :: RandomGen g => Double -> SIRAgentIn -> Rand g Bool
gotInfected infectionProb ain = onDataM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> AgentData SIRMsg -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM infectionProb
    gotInfectedAux x _ = return x

respondToContactWith :: SIRState -> SIRAgentIn -> SIRAgentOut -> SIRAgentOut
respondToContactWith state ain ao = onData respondToContactWithAux ain ao
  where
    respondToContactWithAux :: AgentData SIRMsg -> SIRAgentOut -> SIRAgentOut
    respondToContactWithAux (senderId, Contact _) ao = dataFlow (senderId, Contact state) ao

distributeData :: [(AgentId, AgentOut o d)] -> [AgentIn d]
distributeData aouts = map (distributeDataAux allMsgs) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allMsgs = collectAllData aouts
    ains = map (\(ai, _) -> agentIn ai) aouts 

    distributeDataAux :: Map.Map AgentId [AgentData d]
                      -> AgentIn d
                      -> AgentIn d
    distributeDataAux allMsgs ain = ain'
      where
        receiverId = aiId ain
        msgs = aiData ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverMsgs = Map.lookup receiverId allMsgs
        msgsEvt = maybe msgs (\receiverMsgs -> receiverMsgs ++ msgs) mayReceiverMsgs

        ain' = ain { aiData = msgsEvt }

    collectAllData :: [(AgentId, AgentOut o d)] -> Map.Map AgentId [AgentData d]
    collectAllData aos = foldr collectAllDataAux Map.empty aos
      where
        collectAllDataAux :: (AgentId, AgentOut o d)
                              -> Map.Map AgentId [AgentData d]
                              -> Map.Map AgentId [AgentData d]
        collectAllDataAux (senderId, ao) accMsgs 
            | not $ null msgs = foldr collectAllDataAuxAux accMsgs msgs
            | otherwise = accMsgs
          where
            msgs = aoData ao

            collectAllDataAuxAux :: AgentData d
                                 -> Map.Map AgentId [AgentData d]
                                 -> Map.Map AgentId [AgentData d]
            collectAllDataAuxAux (receiverId, m) accMsgs = accMsgs'
              where
                msg = (senderId, m)
                mayReceiverMsgs = Map.lookup receiverId accMsgs
                newMsgs = maybe [msg] (\receiverMsgs -> (msg : receiverMsgs)) mayReceiverMsgs

                -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
                accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId    = aid
  , aiData  = []
  }

agentOut :: o -> AgentOut o d
agentOut o = AgentOut {
    aoData        = []
  , aoObservable  = o
  }

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

{-
randomExpM :: RandomGen g => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))
  where
    avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r

randomElem :: RandomGen g => [a] -> Rand g a
randomElem as = getRandomR (0, len - 1) >>= (\idx -> return $ as !! idx)
  where
    len = length as
    -}