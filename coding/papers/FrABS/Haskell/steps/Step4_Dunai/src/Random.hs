{-# LANGUAGE Arrows     #-}
module Random ( runRandomSIR ) where

import System.IO

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Random
import qualified Data.Map as Map
import FRP.BearRiver

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

type Agent m o d  = SF m (AgentIn d) (AgentOut o d)

type SIRMonad g   = Rand g
data SIRMsg       = Contact SIRState deriving (Show, Eq)
type SIRAgentIn   = AgentIn SIRMsg
type SIRAgentOut  = AgentOut SIRState SIRMsg
type SIRAgent g   = Agent (SIRMonad g) SIRState SIRMsg

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

runRandomSIR :: IO ()
runRandomSIR = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  let as = initAgents agentCount infectedCount
  let ass = runSimulation g t dt as

  let dyns = aggregateAllStates ass
  let fileName =  "STEP_4_BEARRIVER_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> [(AgentId, SIRState)] 
              -> [[SIRState]]
runSimulation g t dt as = map (\aos -> map aoObservable aos) aoss
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

initAgents :: Int -> Int -> [(AgentId, SIRState)]
initAgents n i = sus ++ inf
  where
    sus = map (\ai -> (ai, Susceptible)) [0..n-i-1]
    inf = map (\ai -> (ai, Infected)) [n-i..n-1]

dataFlow :: AgentData d -> AgentOut o d -> AgentOut o d
dataFlow df ao = ao { aoData = df : aoData ao }

onDataM :: (Monad m) 
        => (acc -> AgentData d -> m acc) 
        -> AgentIn d 
        -> acc 
        -> m acc
onDataM dHdl ai acc = foldM dHdl acc ds
  where
    ds = aiData ai

onData :: (AgentData d -> acc -> acc) -> AgentIn d -> acc -> acc
onData dHdl ai a = foldr (\msg acc'-> dHdl msg acc') a ds
  where
    ds = aiData ai

gotInfected :: RandomGen g => Double -> SIRAgentIn -> Rand g Bool
gotInfected p ain = onDataM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> AgentData SIRMsg -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM p
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