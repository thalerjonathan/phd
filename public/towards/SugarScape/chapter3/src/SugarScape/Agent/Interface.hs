{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Interface
  ( AgentId
  , ABSEvent (..)

  , ABSState (..)
  
  , AgentSF
  , AgentT
  
  , AgentDef (..)
  , AgentOut (..)

  , nextAgentId

  , mkAbsState
  , defaultAbsState
  
  , agentOut
  , agentOutObservable
  
  , sendEventTo
  , sendEventToWithCont

  , isObservable
  , isDead
  , kill
  , newAgent

  , (<°>)
  ) where

import Data.Maybe

import Control.Monad.State.Strict
import FRP.BearRiver

type AgentId    = Int
data ABSEvent e = TimeStep 
                | DomainEvent (AgentId, e) 
                  deriving (Show, Eq) -- sender, event 

data ABSState = ABSState
  { absNextId :: AgentId
  , absTime   :: Time
  } deriving (Show, Eq)

type AgentT m      = StateT ABSState m
type AgentSF m e o = SF (AgentT m) (ABSEvent e) (AgentOut m e o)

data AgentDef m e o = AgentDef
  { adId :: !AgentId
  , adSf :: AgentSF m e o
  }

data AgentOut m e o = AgentOut 
  { aoKill       :: !(Event ())
  , aoCreate     :: ![AgentDef m e o]
  , aoObservable :: !(Maybe o)
  , aoEvents     :: ![(AgentId, e)]                     -- 1-directional event receiver, (DomainEvent) event
  , aoEventWCont :: !(Maybe (AgentId, e, AgentSF m e o))  -- bi-directional event, receiver, (DomainEvent) event, continuation
  }

nextAgentId :: MonadState ABSState m
            => m AgentId
nextAgentId = do
  aid <- gets absNextId
  modify (\s -> s { absNextId = aid + 1 })
  return aid

defaultAbsState :: ABSState
defaultAbsState = mkAbsState 0 

mkAbsState :: AgentId -> ABSState
mkAbsState initId = ABSState 
  { absNextId = initId + 1
  , absTime   = 0
  }

agentOut :: AgentOut m e o
agentOut = agentOutAux Nothing

agentOutObservable :: o -> AgentOut m e o
agentOutObservable o = agentOutAux $ Just o

agentOutAux :: Maybe o -> AgentOut m e o
agentOutAux mo = AgentOut 
  { aoKill       = NoEvent
  , aoCreate     = []
  , aoObservable = mo
  , aoEvents     = []
  , aoEventWCont = Nothing
  }

sendEventTo :: AgentId
            -> e
            -> AgentOut m e o
            -> AgentOut m e o
sendEventTo receiver e ao = ao'
  where
    es  = aoEvents ao
    -- important: respect ordering!
    ao' = ao { aoEvents = es ++ [(receiver, e)] } 

sendEventToWithCont :: AgentId
                    -> e
                    -> AgentSF m e o
                    -> AgentOut m e o
                    -> AgentOut m e o
sendEventToWithCont receiver e cont ao
    | isJust $ aoEventWCont ao = error "Event with continuation already exists!"
    | otherwise                = ao'
  where
    ao' = ao { aoEventWCont = Just (receiver, e, cont) } 

isDead :: AgentOut m e o -> Bool
isDead ao = isEvent $ aoKill ao

kill :: AgentOut m e o -> AgentOut m e o
kill ao = ao { aoKill = Event () }

newAgent :: AgentDef m e o
         -> AgentOut m e o 
         -> AgentOut m e o
newAgent adef ao 
  = ao { aoCreate = adef : aoCreate ao }

isObservable :: AgentOut m e o -> Bool
isObservable ao = isJust $ aoObservable ao

(<°>) :: AgentOut m e o 
      -> AgentOut m e o 
      -> AgentOut m e o
(<°>) ao1 ao2 = AgentOut 
    { aoKill       = mergeBy (\_ _ -> ()) (aoKill ao1) (aoKill ao2)
    , aoCreate     = aoCreate ao1 ++ aoCreate ao2
    , aoObservable = decideMaybe (aoObservable ao1) (aoObservable ao2)
    , aoEvents     = aoEvents ao1 ++ aoEvents ao2
    , aoEventWCont = decideMaybe (aoEventWCont ao1) (aoEventWCont ao2)
    }
  where
    decideMaybe :: Maybe a 
                -> Maybe a 
                -> Maybe a
    decideMaybe Nothing Nothing  = Nothing
    decideMaybe (Just a) Nothing = Just a
    decideMaybe Nothing (Just a) = Just a
    decideMaybe _       _        = error "Can't decide between two Maybes"