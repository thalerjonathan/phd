module FrABS.FrABS where

-- TODO: import all relevant modules and re-export them
import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- TODOs
------------------------------------------------------------------------------------------------------------------------
-- Monadic Conversations

-- Use Dunai to run State Monad

-- TODO: allow to be able to stop simulation when iteration.function returns True

-- TODO: hide AgentIn and AgentOut same way as DTime is hidden, only generic state in/out
-- TODO  can we somehow restrict access to agentin/out? hiding dataconstructors? making them completely opaque?

-- TODO create project structure according to put it on Hackage in september: tests, comments,...
-- TODO write unit-tests
-- TODO write QuickCheck tests

-- TODO STM FrABS using Dunai?

------------------------------------------------------------------------------------------------------------------------
