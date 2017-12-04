{-# LANGUAGE Arrows #-}
module FRP.Chimera.Agent.Agent 
  (
    AgentId
  , AgentData
  , DataFilter
  , AgentObservable

  , Agent
  , AgentRandom

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
  , agentOutObservable
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

type AgentId              = Int
type AgentData d          = (AgentId, d)
type DataFilter d         = AgentData d -> Bool
type AgentObservable o    = (AgentId, o)

type Agent m o d e        = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) e
type AgentRandom o d e g  = Agent (Rand g) o d e 

{--
type AgentConversationReceiver o d e = (AgentIn o d e
                                            -> e
                                            -> AgentData d
                                            -> Maybe (s, m, e)) -- NOTE: the receiver MUST reply, otherwise we could've used the normal messaging
-}

type AgentConversationSender m o d e = (AgentOut m o d e
                                        -> e
                                        -> Maybe (AgentData d)   -- NOTE: this will be Nothing in case the conversation with the target was not established e.g. id not found, target got no receiving handler
                                        -> (AgentOut m o d e, e))

type AgentPureBehaviour m o d e         = e -> Double -> AgentIn o d e -> (AgentOut m o d e, e)
type AgentPureBehaviourReadEnv m o d e  = e -> Double -> AgentIn o d e -> AgentOut m o d e
type AgentPureBehaviourNoEnv m o d e    = Double -> AgentIn o d e -> AgentOut m o d e

data AgentDef m o d e = AgentDef
  { adId           :: !AgentId
  , adBeh          :: Agent m o d e
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

data AgentOut m o d e = AgentOut 
  { aoKill                  :: !(Event ())
  , aoCreate                :: ![AgentDef m o d e]
  , aoData                  :: ![AgentData d]           -- AgentId identifies receiver
  , aoConversationRequest   :: !(Event (AgentData d, AgentConversationSender m o d e))
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

createAgent :: AgentDef m o d e -> AgentOut m o d e -> AgentOut m o d e
createAgent newDef ao = ao { aoCreate = newDef : aoCreate ao }

agentOut:: AgentOut m o d e
agentOut = agentOutAux Nothing

agentOutObservable :: o -> AgentOut m o d e
agentOutObservable o = agentOutAux (Just o)

nextAgentId :: AgentIn o d e -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut m o d e -> AgentOut m o d e
kill ao = ao { aoKill = Event () }

isDead :: AgentOut m o d e -> Bool
isDead = isEvent . aoKill

-------------------------------------------------------------------------------
-- EVENTS
-------------------------------------------------------------------------------
onStart :: (AgentOut m o d e -> AgentOut m o d e) 
        -> AgentIn o d e 
        -> AgentOut m o d e 
        -> AgentOut m o d e
onStart evtHdl ai ao = onEvent evtHdl startEvt ao
  where
    startEvt = aiStart ai

onEvent :: (AgentOut m o d e -> AgentOut m o d e) 
        -> Event () 
        -> AgentOut m o d e 
        -> AgentOut m o d e
onEvent evtHdl evt ao = event ao (\_ -> evtHdl ao) evt

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOW
-------------------------------------------------------------------------------
dataFlow :: AgentData d -> AgentOut m o d e -> AgentOut m o d e
dataFlow d ao = ao { aoData = d : aoData ao }

dataFlowTo :: AgentId -> d -> AgentOut m o d e -> AgentOut m o d e
dataFlowTo aid msg ao = dataFlow (aid, msg) ao

dataFlows :: [AgentData d] -> AgentOut m o d e ->  AgentOut m o d e
dataFlows msgs ao = foldr dataFlow ao msgs

broadcastDataFlow :: d -> [AgentId] -> AgentOut m o d e -> AgentOut m o d e
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
agentObservable :: AgentOut m o d e -> o
agentObservable = fromJust . aoObservable

-- NOTE: assuming that state isJust
updateAgentObservable :: (o -> o) -> AgentOut m o d e -> AgentOut m o d e
updateAgentObservable f ao = 
  ao { aoObservable = Just $ f $ fromJust $ aoObservable ao }

setAgentObservable :: o -> AgentOut m o d e -> AgentOut m o d e
setAgentObservable o ao = updateAgentObservable (const o) ao

-------------------------------------------------------------------------------
-- Conversations
-------------------------------------------------------------------------------
hasConversation :: AgentOut m o d e -> Bool
hasConversation = isEvent . aoConversationRequest

conversation :: AgentData d
             -> AgentConversationSender m o d e
             -> AgentOut m o d e
             -> AgentOut m o d e
conversation msg replyHdl ao = ao { aoConversationRequest = Event (msg, replyHdl)}

conversationEnd :: AgentOut m o d e -> AgentOut m o d e
conversationEnd ao = ao { aoConversationRequest = NoEvent }

-------------------------------------------------------------------------------
-- RECURSION
-------------------------------------------------------------------------------
agentRecursions :: AgentIn o d e -> Event [(AgentObservable o, e)]
agentRecursions = aiRec

recInitAllowed :: AgentIn o d e -> Bool
recInitAllowed = aiRecInitAllowed

allowsRecOthers :: AgentOut m o d e -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut m o d e -> AgentOut m o d e
recursive  allowOthers aout = 
  aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut m o d e -> AgentOut m o d e
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn o d e -> Bool
isRecursive ain = isEvent $ aiRec ain

-------------------------------------------------------------------------------
-- PURE WRAPPERS
-------------------------------------------------------------------------------
agentPure :: Monad m 
          => AgentPureBehaviour m o d e 
          -> Agent m o d e
agentPure f = proc (ain, e) -> do
  t <- time -< ()
  let (_ao, e') = f e t ain
  -- TODO: put ao using state-monad
  returnA -< e'

agentPureReadEnv :: Monad m 
                 => AgentPureBehaviourReadEnv m o d e 
                 -> Agent m o d e
agentPureReadEnv f = proc (ain, e) -> do
  t <- time -< ()
  let _ao = f e t ain
  -- TODO: put ao using state-monad
  returnA -< e

agentPureIgnoreEnv :: Monad m 
                   => AgentPureBehaviourNoEnv m o d e 
                   -> Agent m o d e
agentPureIgnoreEnv f = proc (ain, e) -> do
  t <- time -< ()
  let _ao = f t ain
  -- TODO: put ao using state-monad
  returnA -< e

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
startingAgentIn :: [AgentDef m o d e] -> TVar Int -> [AgentIn o d e]
startingAgentIn adefs idGen = map (startingAgentInFromAgentDef idGen) adefs

startingAgent :: [AgentDef m o d e] 
              -> TVar Int 
              -> ([Agent m o d e], [AgentIn o d e])
startingAgent adefs idGen = (sfs, ains)
  where
    ains = startingAgentIn adefs idGen
    sfs = map adBeh adefs 

startingAgentInFromAgentDef :: TVar Int -> AgentDef m o d e -> AgentIn o d e
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
agentOutAux :: Maybe o -> AgentOut m o d e
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