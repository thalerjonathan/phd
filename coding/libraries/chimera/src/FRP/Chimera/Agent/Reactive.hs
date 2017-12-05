{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Reactive 
  (
    AgentIgnoreEnv
  , AgentReadEnv

  , ignoreEnv
  , readEnv

  {-
  , doOnce
  , doOnceR
  , doNothing
  , doNothingObs
  , doRepeatedlyEvery
  , doOccasionallyEvery

  , setAgentStateR
  , updateAgentStateR
  -}
  ) where

import Data.Maybe

import Control.Monad.State
import Control.Monad.Random
import FRP.BearRiver

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Agent.Monad
import FRP.Chimera.Environment.Discrete
import FRP.Chimera.Environment.Network
import FRP.Chimera.Random.Monadic 
import FRP.Chimera.Random.Reactive

type AgentIgnoreEnv m o d e = SF (StateT (AgentOut m o d e) m) (AgentIn o d e) ()
type AgentReadEnv m o d e   = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) () 

ignoreEnv :: Monad m => AgentIgnoreEnv m o d e -> Agent m o d e 
ignoreEnv f = proc (ain, e) -> do
  _ <- f -< ain
  returnA -< e

readEnv :: Monad m => AgentReadEnv m o d e -> Agent m o d e
readEnv f = proc (ain, e) -> do
  _ <- f -< (ain, e)
  returnA -< e
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------
{-
-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOnce :: MonadState (AgentOut m o d e) m
       => (AgentOut m o d e -> AgentOut m o d e) 
       -> Agent m o d e
doOnce f = proc ao -> do
  -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
  -- TODO: why is this not firing the first time?
  aoOnceEvt <- once -< (Event . f) ao
  let ao' = event ao id aoOnceEvt
  returnA -< ao'

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOnceR :: s -> Agent m o d e -> Agent m o d e
doOnceR s sf = proc (ain, e) -> do
  doEvt <- once -< (Event ())
  if (isEvent doEvt) then (do
    (aout, e') <- sf -< (ain, e)
    returnA -< (aout, e'))
    else 
      returnA -< (agentOutObs s, e)

doNothing :: Agent m o d e
doNothing = first $ arr (const agentOut)

doNothingObs :: s -> Agent m o d e
doNothingObs s = first $ arr (const $ agentOutObs s)

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
setAgentStateR :: s -> Agent m o d e
setAgentStateR s = first $ arr ((setAgentState s) . (const $ agentOutObs s))

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
updateAgentStateR :: s -> (s -> s) -> Agent m o d e
updateAgentStateR s f = first $ arr ((updateAgentState f) . (const $ agentOutObs s))

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doRepeatedlyEvery :: Time -> s -> Agent m o d e -> Agent m o d e
doRepeatedlyEvery t s sf = proc (ain, e) -> do
  doEvt <- repeatedly t () -< ()
  if (isEvent doEvt) then (do
    (aout', e') <- sf -< (ain, e)
    returnA -< (aout', e'))
    else 
      returnA -< (agentOutObs s, e)

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOccasionallyEvery :: RandomGen g => g -> Time -> s -> Agent m o d e -> Agent m o d e
doOccasionallyEvery g t s sf = proc (ain, e) -> do
  doEvt <- occasionally g t () -< ()
  if (isEvent doEvt) then (do
    (aout', e') <- sf -< (ain, e)
    returnA -< (aout', e'))
    else 
      returnA -< (agentOutObs s, e)
      -}
-------------------------------------------------------------------------------