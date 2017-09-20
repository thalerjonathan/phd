{-# LANGUAGE Arrows #-}
module NewAgents.Agent (
    newAgentBehaviour
  ) where

import NewAgents.Model

import FRP.FrABS
import FRP.Yampa

import Data.Maybe
import Data.List
import System.Random
import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
newAgentBehaviour :: NewAgentState -> NewAgentBehaviour
newAgentBehaviour s = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        
        returnA -< (ao, e)
------------------------------------------------------------------------------------------------------------------------