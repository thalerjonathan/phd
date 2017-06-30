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

-- TODO: need some mechanism to give GLOBALLY UNIQUE IDs to agents when creating new
  -- how can we handle the parallel-case?
    {-
        how do we generate global unique agentids when creating them during runtime?
        we could let the runtimesystem handle it because it knows all agents but then how can the
         parent-agent get to know which ids they have? this is important as agents can only communicate
         with each other by knowing their ids. we could introduce "initialMessages" in the agentdef
         which allows the parent to send initial messages to the child. this allows the domain-specific
          handling of new children: if a parent needs to know the id of its children and/or the children
          the parents' id then the parent sends a domain-specific message in initial messages with its
          id and the child replies with a same message and maybe a tag which allows the parent to distinguish
          the child from other newly created ones. this is not required in sugarscape thus no initial messages
           are placed but it may be necessary in Heroes and cowards when creating dynamically new agents
        -}

-- TODO: unique Ids through the state-monad?

-- TODO: allow to be able to stop simulation when iteration.function returns True
-- TODO: sequential iteration should have the feature to shuffle agents randomly before iterating them

-- TODO: hide AgentIn and AgentOut same way as DTime is hidden, only generic state in/out
-- TODO  can we somehow restrict access to agentin/out? hiding dataconstructors? making them completely opaque?

-- TODO create project structure according to put it on Hackage in september: tests, comments,...
-- TODO write unit-tests
-- TODO write QuickCheck tests

-- TODO STM FrABS using Dunai?

------------------------------------------------------------------------------------------------------------------------
