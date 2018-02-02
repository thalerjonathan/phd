TimeDelta : Type
TimeDelta = Double

mutual
  data Agent : Type -> Type where
    -- TODO: can we express virtual simulation time in dependent types different?
    -- e.g. guarantee a fixed dt in types which guarantees a next time-step with t+dt?
    TimeStep : AgentCmd -> Agent TimeDelta

    -- initializes an agent with a given state
    --Pure    : a -> Agent a
    -- executes agents sequentially
    Bind    : Agent a -> (a -> Agent b) -> Agent b

  data AgentCmd : Type -> Type where
    -- TODO commands for
    --   random numbers
    --   local agent state manipulation
    --   global environment manipulation
    --   data-flow in/out
    --   setting of observable data (no updating, it should not be abused as a placeholder of some state)
    --   time-dependent event-generation functions: after, occasionally
    --   type-safe agent-transactions

    -- probably this should be the setting of observable data?
    Pure  : a -> AgentCmd a
    (>>=) : AgentCmd a -> (a -> AgentCmd b) -> AgentCmd b

namespace AgentDo
  (>>=) : Agent a -> (a -> Agent b) -> Agent b
  (>>=) = Bind 

-- TODO: must not run in IO when finished
runAgentCommand : AgentCmd a -> IO a
runAgentCommand (Pure x) = ?runAgentCommands_rhs_1
runAgentCommand (x >>= f) = ?runAgentCommands_rhs_2

runAgent : Agent a -> IO a
runAgent (TimeStep x) = ?runAgent_rhs_1

sirAgent : Agent a 
sirAgent = do
  dt <- TimeStep
  sirAgent