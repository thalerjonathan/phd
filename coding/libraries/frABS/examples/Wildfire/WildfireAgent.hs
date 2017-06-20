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
rngOfAgent :: SF WildfireAgentOut StdGen
rngOfAgent = arr aoRng

igniteNeighbours :: WildfireAgentOut -> WildfireAgentOut
igniteNeighbours ao = sendMessage a' (n, Ignite)
	where
		nids = neighbourIds ao
		(n, a') = agentPickRandom ao nids

igniteNeighboursSF :: RandomGen g => g -> SF WildfireAgentOut WildfireAgentOut
igniteNeighboursSF g = proc a-> 
	do
		g <- rngOfAgent -< a
		ignitionEvent <- occasionally g 2.0 () -< ()
		returnA -< (event a (\_ -> igniteNeighbours a) ignitionEvent)

isBurnedDown :: WildfireAgentOut -> Bool
isBurnedDown ao = wfFuel s <= 0.0
	where
		s = aoState ao

-- TODO: burning down per time-unit !!
burndown :: WildfireAgentOut -> WildfireAgentOut
burndown ao = updateState ao (\s -> s { wfLifeState = Burning, wfFuel = fuel'}) 
	where
		s = aoState ao
		fuel = wfFuel s
		fuel' = max 0.0 (fuel - 0.1)

burndownSF :: SF WildfireAgentOut WildfireAgentOut
burndownSF = proc a ->
	do
		let s = aoState a
		let fuel = wfFuel s
		remainingFuel <- (1-) ^<< integral -< fuel
		returnA -< updateState a (\s -> s { wfLifeState = Burning, wfFuel = (max 0.0 remainingFuel)}) 

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
		ignitionEvent <- (iEdge False) -< hasMessage ain Ignite 	-- NOTE: need to use iEdge False to detect Ignite messages which were added at time 0: at initialization time
		returnA -< (aout, ignitionEvent)

wildfireAgentIgnite :: RandomGen g => g -> () -> WildfireAgentBehaviour
wildfireAgentIgnite g evt = wildfireAgentBurningBehaviour g

wildfireAgentDie :: () -> WildfireAgentBehaviour
wildfireAgentDie evt = proc ain ->
	do
		let aout = agentOutFromIn ain
		let aout' = updateState aout (\s -> s { wfLifeState = Dead })
		returnA -< aout' -- kill aout' -- NOTE: killing would lead to increased performance but would leave the patch white (blank)

wildfireAgentBurning :: RandomGen g => g -> SF WildfireAgentIn (WildfireAgentOut, Event ())
wildfireAgentBurning g = proc ain -> 
	do
		let a = agentOutFromIn ain
		
		a' <- burndownSF -< a
		a'' <- igniteNeighboursSF g -< a'

		dyingEvent <- edge -< isBurnedDown a''

		returnA -< (a'', dyingEvent)

wildfireAgentBurningBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentBurningBehaviour g = switch (wildfireAgentBurning g) wildfireAgentDie

wildfireAgentLivingBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentLivingBehaviour g = switch wildfireAgentLiving (wildfireAgentIgnite g)
------------------------------------------------------------------------------------------------------------------------