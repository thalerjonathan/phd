module FrABS.FrABS where

-- TODO: import all relevant modules and re-export them
import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- TODOs
------------------------------------------------------------------------------------------------------------------------
-- TODO: if there is no use of the environment but one is running parallel it is strange to provide a collapse function

-- TODO: need some mechanism to give GLOBALLY UNIQUE IDs to agents when creating new
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

-- TODO: implement replications (using parallelism!)

-- TODO: allow to be able to stop simulation when iteration.function returns True
-- TODO: sequential iteration should have the feature to shuffle agents randomly before iterating them

-- TODO: hide AgentIn and AgentOut same way as DTime is hidden, only generic state in/out

-- TODO create project structure according to put it on Hackage in september: tests, comments,...
-- TODO write unit-tests
-- TODO write QuickCheck tests

-- TODO STM FrABS using Dunai?
{- TODO: do we really need Yampa?
    The essences are:
    -> continuations
    -> arrows
    -> Events which are the same as the Maybe-type
    -> Would be great to have some kind of monad to work on agents to have some kind of 'imperative' style on agents
    -> Would be great to introduce STM as well
-}

------------------------------------------------------------------------------------------------------------------------
