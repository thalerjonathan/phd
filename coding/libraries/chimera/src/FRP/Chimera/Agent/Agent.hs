{-# LANGUAGE Arrows #-}
module FRP.Chimera.Agent.Agent 
  (
    AgentId
  , AgentData
  , DataFilter
  , AgentObservable

  , AgentBehaviour
  , AgentBehaviourRandom

  , AgentConversationSender

  , AgentPureBehaviour
  , AgentPureBehaviourReadEnv
  , AgentPureBehaviourNoEnv

  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)

  , agentId
  , createAgent
  , kill
  , isDead
  , agentOut
  , agentOutObs
  , nextAgentId

  , onStart
  , onEvent

  , dataFlow
  , dataFlowTo
  , dataFlows
  , broadcastDataFlow
  , hasDataFlow
  , onDataFlow
  , onFilterDataFlow
  , onDataFlowFrom
  , onDataFlowType

  , hasConversation
  , conversation
  , conversationEnd

  , agentObservable
  , updateAgentObservable
  , setAgentObservable

  , recInitAllowed
  , allowsRecOthers
  , recursive
  , unrecursive
  , isRecursive
  , agentRecursions

  , agentPure
  , agentPureReadEnv
  , agentPureIgnoreEnv

  , startingAgent
  , startingAgentIn
  , startingAgentInFromAgentDef
  ) where

import Data.List
import Data.Maybe

import Control.Concurrent.STM.TVar
import Control.Monad.Random
import Control.Monad.State
import FRP.BearRiver

import FRP.Chimera.Simulation.Internal

type AgentId                  = Int
type AgentData d              = (AgentId, d)
type DataFilter d             = AgentData d -> Bool
type AgentObservable o        = (AgentId, o)

type AgentBehaviour o d e m       = SF (StateT (AgentOut o d e m) m) (AgentIn o d e, e) e
type AgentBehaviourRandom o d e g = AgentBehaviour o d e (Rand g)

{--
type AgentConversationReceiver o d e = (AgentIn o d e
                                            -> e
                                            -> AgentData d
                                            -> Maybe (s, m, e)) -- NOTE: the receiver MUST reply, otherwise we could've used the normal messaging
-}

type AgentConversationSender o d e m = (AgentOut o d e m
                                        -> e
                                        -> Maybe (AgentData d)   -- NOTE: this will be Nothing in case the conversation with the target was not established e.g. id not found, target got no receiving handler
                                        -> (AgentOut o d e m, e))

type AgentPureBehaviour o d e m         = e -> Double -> AgentIn o d e -> (AgentOut o d e m, e)
type AgentPureBehaviourReadEnv o d e m  = e -> Double -> AgentIn o d e -> AgentOut o d e m
type AgentPureBehaviourNoEnv o d e m    = Double -> AgentIn o d e -> AgentOut o d e m

data AgentDef o d e m = AgentDef
  { adId           :: !AgentId
  , adBeh          :: AgentBehaviour o d e m
  , adInitData     :: ![AgentData d]     -- AgentId identifies sender
  }

data AgentIn o d e = AgentIn 
  { aiId                    :: !AgentId
  , aiData                  :: ![AgentData d]     -- AgentId identifies sender
  , aiConversationIncoming  :: !(Event (AgentData d))
  , aiStart                 :: !(Event ())
  , aiRec                   :: !(Event [(AgentObservable o, e)])
  , aiRecInitAllowed        :: !Bool
  , aiIdGen                 :: !(TVar Int)
  }

data AgentOut o d e m = AgentOut 
  { aoKill                  :: !(Event ())
  , aoCreate                :: ![AgentDef o d e m]
  , aoData                  :: ![AgentData d]           -- AgentId identifies receiver
  , aoConversationRequest   :: !(Event (AgentData d, AgentConversationSender o d e m))
  , aoConversationReply     :: !(Event (AgentData d))
  , aoObservable            :: !(Maybe o)             -- OPTIONAL observable state
  , aoRec                   :: !(Event ())
  , aoRecOthersAllowed      :: !Bool
  }

-------------------------------------------------------------------------------
-- GENERAL 
-------------------------------------------------------------------------------
agentId :: AgentIn o d e -> AgentId
agentId = aiId 

createAgent :: AgentDef o d e m -> AgentOut o d e m -> AgentOut o d e m
createAgent newDef ao = ao { aoCreate = newDef : aoCreate ao }

agentOut:: AgentOut o d e m
agentOut = agentOutAux Nothing

agentOutObs :: o -> AgentOut o d e m
agentOutObs o = agentOutAux (Just o)

nextAgentId :: AgentIn o d e -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut o d e m -> AgentOut o d e m
kill ao = ao { aoKill = Event () }

isDead :: AgentOut o d e m -> Bool
isDead = isEvent . aoKill

-------------------------------------------------------------------------------
-- EVENTS
-------------------------------------------------------------------------------
onStart :: (AgentOut o d e m -> AgentOut o d e m) 
        -> AgentIn o d e 
        -> AgentOut o d e m 
        -> AgentOut o d e m
onStart evtHdl ai ao = onEvent evtHdl startEvt ao
  where
    startEvt = aiStart ai

onEvent :: (AgentOut o d e m -> AgentOut o d e m) 
        -> Event () 
        -> AgentOut o d e m 
        -> AgentOut o d e m
onEvent evtHdl evt ao = event ao (\_ -> evtHdl ao) evt

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOW
-------------------------------------------------------------------------------
dataFlow :: AgentData d -> AgentOut o d e m -> AgentOut o d e m
dataFlow d ao = ao { aoData = d : aoData ao }

dataFlowTo :: AgentId -> d -> AgentOut o d e m -> AgentOut o d e m
dataFlowTo aid msg ao = dataFlow (aid, msg) ao

dataFlows :: [AgentData d] -> AgentOut o d e m ->  AgentOut o d e m
dataFlows msgs ao = foldr dataFlow ao msgs

broadcastDataFlow :: d -> [AgentId] -> AgentOut o d e m -> AgentOut o d e m
broadcastDataFlow d receiverIds ao = dataFlows datas ao
  where
    n = length receiverIds
    ds = replicate n d
    datas = zip receiverIds ds

hasDataFlow :: (Eq d) => d -> AgentIn o d e -> Bool
hasDataFlow d ai = Data.List.any ((==d) . snd) (aiData ai)

onDataFlow :: (AgentData d -> acc -> acc) -> AgentIn o d e -> acc -> acc
onDataFlow dataHdl ai a = foldr (\d acc'-> dataHdl d acc') a ds
  where
    ds = aiData ai

onFilterDataFlow :: DataFilter d 
                  -> (AgentData d -> acc -> acc) 
                  -> AgentIn o d e 
                  -> acc 
                  -> acc
onFilterDataFlow dataFilter dataHdl ai acc =
    foldr (\d acc'-> dataHdl d acc') acc dsFiltered
  where
    ds = aiData ai
    dsFiltered = filter dataFilter ds

onDataFlowFrom :: AgentId 
              -> (AgentData d -> acc -> acc) 
              -> AgentIn o d e 
              -> acc 
              -> acc
onDataFlowFrom senderId datHdl ai acc = 
    onFilterDataFlow filterBySender datHdl ai acc
  where
    filterBySender = (\(senderId', _) -> senderId == senderId' )

onDataFlowType :: (Eq d) 
                => d 
                -> (AgentData d -> acc -> acc) 
                -> AgentIn o d e 
                -> acc 
                -> acc
onDataFlowType d datHdl ai acc = onFilterDataFlow filterByType datHdl ai acc
  where
      filterByType = (==d) . snd 

-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming that state isJust
agentObservable :: AgentOut o d e m -> o
agentObservable = fromJust . aoObservable

-- NOTE: assuming that state isJust
updateAgentObservable :: (o -> o) -> AgentOut o d e m -> AgentOut o d e m
updateAgentObservable f ao = 
  ao { aoObservable = Just $ f $ fromJust $ aoObservable ao }

setAgentObservable :: o -> AgentOut o d e m -> AgentOut o d e m
setAgentObservable o ao = updateAgentObservable (const o) ao

-------------------------------------------------------------------------------
-- Conversations
-------------------------------------------------------------------------------
hasConversation :: AgentOut o d e m -> Bool
hasConversation = isEvent . aoConversationRequest

conversation :: AgentData d
                -> AgentConversationSender o d e m
                -> AgentOut o d e m
                -> AgentOut o d e m
conversation msg replyHdl ao = ao { aoConversationRequest = Event (msg, replyHdl)}

conversationEnd :: AgentOut o d e m -> AgentOut o d e m
conversationEnd ao = ao { aoConversationRequest = NoEvent }

-------------------------------------------------------------------------------
-- RECURSION
-------------------------------------------------------------------------------
agentRecursions :: AgentIn o d e -> Event [(AgentObservable o, e)]
agentRecursions = aiRec

recInitAllowed :: AgentIn o d e -> Bool
recInitAllowed = aiRecInitAllowed

allowsRecOthers :: AgentOut o d e m -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut o d e m -> AgentOut o d e m
recursive  allowOthers aout = 
  aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut o d e m -> AgentOut o d e m
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn o d e -> Bool
isRecursive ain = isEvent $ aiRec ain

-------------------------------------------------------------------------------
-- PURE WRAPPERS
-------------------------------------------------------------------------------
agentPure :: Monad m => AgentPureBehaviour o d e m -> AgentBehaviour o d e m
agentPure f = proc (ain, e) -> do
  t <- time -< ()
  let (_ao, e') = f e t ain
  -- TODO: put ao using state-monad
  returnA -< e'

agentPureReadEnv :: Monad m => AgentPureBehaviourReadEnv o d e m -> AgentBehaviour o d e m
agentPureReadEnv f = proc (ain, e) -> do
  t <- time -< ()
  let _ao = f e t ain
  -- TODO: put ao using state-monad
  returnA -< e

agentPureIgnoreEnv :: Monad m => AgentPureBehaviourNoEnv o d e m -> AgentBehaviour o d e m
agentPureIgnoreEnv f = proc (ain, e) -> do
  t <- time -< ()
  let _ao = f t ain
  -- TODO: put ao using state-monad
  returnA -< e

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
startingAgentIn :: [AgentDef o d e m] -> TVar Int -> [AgentIn o d e]
startingAgentIn adefs idGen = map (startingAgentInFromAgentDef idGen) adefs

startingAgent :: [AgentDef o d e m] 
              -> TVar Int 
              -> ([AgentBehaviour o d e m], [AgentIn o d e])
startingAgent adefs idGen = (sfs, ains)
  where
    ains = startingAgentIn adefs idGen
    sfs = map adBeh adefs 

startingAgentInFromAgentDef :: TVar Int -> AgentDef o d e m -> AgentIn o d e
startingAgentInFromAgentDef idGen ad = 
  AgentIn { aiId = adId ad
          , aiData = adInitData ad
          , aiConversationIncoming = NoEvent
          , aiStart = Event ()
          , aiRec = NoEvent
          , aiRecInitAllowed = True
          , aiIdGen = idGen 
          }

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
agentOutAux :: Maybe o -> AgentOut o d e m
agentOutAux o = 
  AgentOut {  aoKill = NoEvent
            , aoCreate = []
            , aoData = []
            , aoConversationRequest = NoEvent
            , aoConversationReply = NoEvent
            , aoObservable = o
            , aoRec = NoEvent
            , aoRecOthersAllowed = True
            } 