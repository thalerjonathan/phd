module FrABS.FrABS where


import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- TODOs
------------------------------------------------------------------------------------------------------------------------
-- PROJECT-STRUCTURE
	-- import all relevant modules and re-export them: have it all available through single import of FrABS.FrABS

-- provide a runAgentMonadic function which requires a function (Double -> AgentIn -> State AgentOut ()) which allows to completely run an agent within the state monad and having the current age of the agent and input available
-- study arrowized programming (papers): how can dt disappear ? can we ommit arguments which are implicitly there?
-- develop arrowized EDSL for ABS: timeout transitions, rate transitions, sending messages after, repeatedly send message in interval, occasionally send message

-- TODO create project structure according to put it on Hackage in september: tests, comments,...
-- TODO write unit-tests
-- TODO write QuickCheck tests

-- Monadic Conversations

-- Use Dunai to run State Monad

-- TODO: allow to be able to stop simulation when iteration.function returns True

-- TODO: hide AgentIn and AgentOut same way as DTime is hidden, only generic state in/out
-- TODO  can we somehow restrict access to agentin/out? hiding dataconstructors? making them completely opaque?

-- TODO STM FrABS using Dunai?
------------------------------------------------------------------------------------------------------------------------