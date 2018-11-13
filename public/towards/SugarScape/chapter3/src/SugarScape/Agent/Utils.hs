{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Utils 
  ( continueWithAfter

  , absStateLift
  , envLift
  , randLift
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict

import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

import SugarScape.Agent.Interface
import SugarScape.Model


-- this is a simplified version of switch WITH DIFFERENT SEMANTICS:
-- at the time of the switching the new MSF is NOT activated but only in the next step!

continueWithAfter :: Monad m => MSF m a (b, Maybe (MSF m a b)) -> MSF m a b
continueWithAfter msf = MSF $ \a -> do
  ((b, msfCont), msf') <- unMSF msf a
  let msfNext = fromMaybe (continueWithAfter msf') msfCont
  return (b, msfNext)

absStateLift :: StateT ABSState (StateT SugEnvironment (Rand g)) a
             -> StateT SugAgentState (StateT ABSState (StateT SugEnvironment (Rand g))) a
absStateLift = lift 

envLift :: StateT SugEnvironment (Rand g) a
        -> StateT SugAgentState (StateT ABSState (StateT SugEnvironment (Rand g))) a
envLift = lift . lift

randLift :: Rand g a
         -> StateT SugAgentState (StateT ABSState (StateT SugEnvironment (Rand g))) a
randLift = lift . lift . lift