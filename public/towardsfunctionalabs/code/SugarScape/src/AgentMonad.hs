{-# LANGUAGE FlexibleInstances #-}
module AgentMonad
  (
    AgentId
  , MonadAgent (..)

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