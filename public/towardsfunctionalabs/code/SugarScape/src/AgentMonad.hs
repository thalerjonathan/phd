{-# LANGUAGE FlexibleInstances #-}
module AgentMonad
  (
    AgentId

  , MonadAgent (..)

  , Agent
  , AgentCont
  , AgentTX

  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)

  , mkAbsState
  ) where

import Control.Monad.State.Strict
import FRP.BearRiver

type AgentId = Int

class (Monad m) => MonadAgent m where
  nextAgentId :: m AgentId
  now :: m Time

data ABSState = ABSState
  { absNextId   :: AgentId
  , absTime     :: Time
  }

instance (MonadAgent m) => MonadAgent (StateT ABSState m) where
  -- nextAgentId :: m AgentId
  nextAgentId = do
    aid <- gets absNextId
    modify (\s -> s { absNextId = aid + 1 })
    return aid

  -- now :: m Time
  now = gets absTime

-- type AgentMonadState m e = StateT (ABSState e) m

mkAbsState :: ABSState
mkAbsState = ABSState 
  { absNextId = 0
  , absTime   = 0
  }

-- an agent is simply a SF with a generic computational context, which depends on the model
-- note that it is important that we do not fix m e.g. to StateT to allow an environment
-- or adding a RandT for allowing randomness but we leave this to the model implementer, otherwise
-- we would burden the API with details (type of the state in StateT, type of the RandomNumber generator 
-- in RandT) they may not need e.g. there are models which do not need a global read/write environment
-- or event don't use randonmness (e.g. SD emulation)
type AgentCont m o = SF (MonadAgent m) AgentIn (AgentOut m o)
type Agent m o     = AgentId -> (MonadAgent m) (AgentCont m o)

data AgentDef m o = AgentDef
  { adId       :: !AgentId
  , adBeh      :: Agent m o
  }

data AgentIn = AgentIn { }

data AgentOut m o = AgentOut 
  { aoKill       :: !(Event ())
  , aoCreate     :: ![AgentDef m o]
  , aoObservable :: !(Maybe o)             -- OPTIONAL observable state
  }