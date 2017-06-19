{-# LANGUAGE Arrows #-}
module Wildfire.WildfireAgent where

-- Project-internal import first
import Wildfire.WildfireModel

import FrABS.Env.Environment
import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List
import System.Random
import Control.Monad.Random
import Control.Monad
import qualified Data.Map as Map

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
igniteNeighbours :: WildfireAgentOut -> WildfireAgentOut
igniteNeighbours ao = broadcastMessage ao Ignite nids -- TODO: use occasionally
	where
		nids = neighbourIds ao

isBurnedDown :: WildfireAgentOut -> Bool
isBurnedDown ao = wfFuel s <= 0.0
	where
		s = aoState ao

burndown :: WildfireAgentOut -> WildfireAgentOut
burndown ao = updateState ao (\s -> s { wfLifeState = Burning, wfFuel = fuel'})
	where
		s = aoState ao
		fuel = wfFuel s
		fuel' = max 0.0 (fuel - 0.1)

neighbourIds :: WildfireAgentOut -> [AgentId]
neighbourIds ao = map snd neighs
	where
		env = aoEnv ao
		coord = aoEnvPos ao
		neighs = neighbours env coord


wildfireAgentLiving :: SF WildfireAgentIn (WildfireAgentOut, Event ())
wildfireAgentLiving = proc ain ->
	do
		let aout = agentOutFromIn ain
		let ignitionEvent = hasMessage ain Ignite
		returnA -< (aout, ignitionEvent)

wildfireAgentIgnite :: () -> WildfireAgentBehaviour
wildfireAgentIgnite evt = wildfireAgentBurningBehaviour

wildfireAgentDie :: () -> WildfireAgentBehaviour
wildfireAgentDie evt = proc ain ->
	do
		let aout = agentOutFromIn ain
		let aout' = updateState aout (\s -> s { wfLifeState = Dead })
		returnA -< aout' -- kill aout' -- NOTE: killing would lead to increased performance but would leave the patch white (blank)

wildfireAgentBurning :: SF WildfireAgentIn (WildfireAgentOut, Event ())
wildfireAgentBurning = proc ain -> 
	do
		let ao = agentOutFromIn ain
		let ao' = burndown ao
		let ao'' = igniteNeighbours ao'
		let dyingEvent = if isBurnedDown ao' then Event () else NoEvent

		returnA -< (ao'', dyingEvent)

wildfireAgentBurningBehaviour :: WildfireAgentBehaviour
wildfireAgentBurningBehaviour = switch wildfireAgentBurning wildfireAgentDie

wildfireAgentLivingBehaviour :: WildfireAgentBehaviour
wildfireAgentLivingBehaviour = switch wildfireAgentLiving wildfireAgentIgnite
------------------------------------------------------------------------------------------------------------------------