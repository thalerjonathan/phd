module SIRInteraction

import Debug.Trace

||| This is basically a dependently typed emulation of method calls,
||| which is one of the three interaction mechanisms available in 
||| Agent-Based Simulation.
||| The workings are as follows
||| 1. an agent wants to start an interaction with another agent,
|||    which opens up a synchronous bi-directional 'channel', when
|||    the target agent exists in the system
||| 2. the initiating agent then sends the first request 
||| 3. the receiving agent MUST send a response and can also update its internal state
||| 4. the initiating agent receives the response, can update its internal state
|||    and may send additional requests or not
||| 5. the system supports rollback and commits: upon a rollback
|||    the state-changes are made obsolete and the system-state is
|||    reverted to when the TX was started, commit accepts all state
|||    changes

AID : Type
AID = Int

data SIRState 
  = Susceptible
  | Infected
  | Recovered

data AgentTransact : Type ->
                     (msg : Type) ->
                     Type where
  
  -- TODO: a Request ALWAYS has a Response
  Request  : msg -> (msg -> AgentTransact msg msg) -> AgentTransact msg msg
  Response : (msg -> AgentTransact a msg) -> AgentTransact a msg

  -- TODO: need a commit
  -- TODO: need an abort
{-
  Action : IO a -> AgentTransact a msg 
  Pure   : a -> AgentTransact a msg
  (>>=)  : AgentTransact a msg -> 
           (a -> AgentTransact b msg) -> 
           AgentTransact b msg
-}

interface Agent (m : Type -> Type) where
  -- TODO: if we provide a proof that the agent exists, we can omit maybe?
  -- TODO: only susceptible agents can make interactions
  startInteract : (aid : AID) -> m (Maybe (AgentTransact a msg)) -- TODO: this one needs to start with a Request
  -- TODO: only infected agents can react to interactions
  checkInteract : (aid : AID) -> m (Maybe (AgentTransact a msg)) -- TODO: this one needs to start with a Response

data SIRInteraction : Type where
  Contact : SIRState -> SIRInteraction

sirAgent : (Agent m) =>
         