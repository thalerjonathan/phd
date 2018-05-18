module ABSMonad

-- base
import Data.Vect
import Debug.Trace
-- contrib
import Data.SortedMap
-- project-local
import SortedQueue

%default total

public export
AgentId : Type 
AgentId = Nat

mutual
  export
  -- TODO: enforce that Event has to be the first call
  -- TODO: add time
  data Agent : (m : Type -> Type) -> 
              (ty : Type) ->
              (evt : Type) -> Type where
              --(t : Nat) -> Type where
    ||| the monadic operations Pure and Bind for sequencing operations
    Pure : (result : ty) -> Agent m ty evt
    Bind : Agent m a evt -> 
          ((result : a) -> Agent m b evt) ->
          Agent m b evt

    ||| Lifting an action of the underlying computational contex into the agent monad
    Lift : Monad m => m ty -> Agent m ty evt

    ||| gets current simulation time
    Time : Agent m Nat evt

    -- TODO: the existence of an AgentId is not guaranteed and depends on time as agents
    -- can be created and terminated 
    ||| Schedules an event to be received by the receiver after t steps
    Schedule : (event : evt) -> (receiver : AgentId) -> (t : Nat) -> Agent m ty evt

    -- NOTE: doesnt work like this, nothing protects us from using Step multiple times within an agent, when the first one is happening,
    -- it will ignore all other step calls after
    -- ||| Stepping operation: for every time-step run this function, need an a for returning observable data for current step
    Step : a -> ((t : Nat) -> Agent m a evt) -> Agent m a evt

    ||| Terminate the agent, need an a for returning observable data for current step
    Terminate : Agent m () evt
    ||| Terminate the agent, need an a for returning observable data for current step
    Spawn : Agent m a evt -> Agent m () evt

    ||| Just an empty operation, for testing purposes, will be removed in final version
    NoOp : Agent m () evt

    ||| behave from now on like the new agent func
    Behaviour : AgentFunc m ty evt -> Agent m () evt

  -- TODO: define reactive operations, build them on Schedule primitive
  --After : (td : Nat) -> Agent m a (t + td)
  --Occasionally : (td : Nat) -> Agent m a (t + td)

  AgentFunc : (m : Type -> Type) -> 
              (ty : Type) ->
              (evt : Type) -> Type 
  AgentFunc m ty evt = (AgentId, evt) -> Agent m ty evt

export
Show (Agent m a evt) where
  show (Pure result) = "Pure"
  show (Bind x f) = "Bind"
  show (Lift x) = "Lift"
  show (Step a f) = "Step"
  show Terminate = "Terminate"
  show (Spawn ag) = "Spawn"
  show NoOp = "NoOp"
  show Time = "Time"
  show (Schedule _ _ _) = "Schedule"
  show (Behaviour _) = "Behaviour"

EventQueue : (evt : Type) -> Type
EventQueue evt = SortedMap Nat (AgentId, evt)

runAgent : Monad m =>
           AgentId ->
           Agent m a evt -> 
           (t : Nat) ->
           (term : Bool) ->
           (events : EventQueue evt) ->
           m (a, Agent m a evt, Bool, EventQueue evt)
runAgent aid ag@(Pure res) t term events = pure (res, ag, term, events)
runAgent aid ag@(Bind act cont) t term events = do
  (ret, ag', term', events') <- runAgent aid act t term events
  runAgent aid (cont ret) t (term || term') events'
runAgent aid ag@(Lift act) t term events = do
  res <- act
  pure (res, ag, term, events)
runAgent aid (Step a f) t term events = do
  -- TODO should we really execute this in this time-step? seems to be wrong
  let t' = (S t)
  let ag = f t'
  pure (a, ag, term, events)
runAgent aid NoOp t term events
  = pure ((), NoOp, term, events)
runAgent aid ag@(Spawn newAg) t term events = do 
  -- TODO: add new agent
  pure ((), ag, term, events)
runAgent aid ag@(Terminate) t term events
  = pure ((), ag, True, events)
runAgent aid Time t term  events
  = pure (t, Time, term, events)
runAgent aid (Schedule event receiver td) t term events = do
  ?runAgent_rhs_1
runAgent aid (Behaviour bf) t term events = do
  ?runAgent_rhs_2

runAgents : Monad m =>
            (t : Nat) ->
            Vect n (AgentId, Agent m ty evt) ->
            m (n' ** Vect n' (AgentId, Agent m ty evt))
runAgents t [] = pure (_ ** [])
runAgents t ((aid, ag) :: as) = do
  (ret, ag', term, events') <- runAgent aid ag t False empty
  (n' ** as') <- runAgents t as
  case term of
    False => pure $ (_ ** (aid, ag') :: as')
    True  => pure $ (_ ** as')

export
runAgentsUntil : Monad m =>
                 (tLimit : Nat) ->
                 Vect n (AgentId, Agent m ty evt) ->
                 m (n' ** Vect n' (AgentId, Agent m ty evt))
runAgentsUntil tLimit as = runAgentsUntilAux tLimit Z empty as
  where
    runAgentsUntilAux : Monad m =>
                        (tLimit : Nat) ->
                        (t : Nat) ->
                        (events : EventQueue evt) -> 
                        Vect n (AgentId, Agent m ty evt) ->
                        m (n' ** Vect n' (AgentId, Agent m ty evt))
    runAgentsUntilAux Z _ events as = pure (_ ** as)
    runAgentsUntilAux (S tLimit) t events as = do
      (n' ** as') <- runAgents t as
      -- no more agents left, simulation is over
      if n' == 0
        then pure (_ ** as')
        else runAgentsUntilAux tLimit (S t) events as' 

Event : (evt : Type) -> Type
Event evt = (AgentId, evt)

simulateUntil : Monad m =>
                (tLimit : Nat) ->
                Vect n (AgentId, AgentFunc m ty evt) ->
                List (Nat, Event evt) -> 
                m (n' ** Vect n' (AgentId, AgentFunc m ty evt))
simulateUntil tLimit as initEvents = 
    let evtQueue = Data.SortedMap.fromList initEvents
    in  simulateUntilAux evtQueue as
  where
    simulateUntilAux : Monad m =>
                       (events : EventQueue evt) -> 
                       Vect n (AgentId, AgentFunc m ty evt) ->
                       m (n' ** Vect n' (AgentId, AgentFunc m ty evt))
    simulateUntilAux evtQueue as = do
      ?simulateUntilAux_rhs1

export
pure : (result : ty) -> 
       Agent m ty evt
pure = Pure

export
(>>=) : Agent m a evt -> 
        ((result : a) -> Agent m b evt) ->
        Agent m b evt
(>>=) = Bind

export
lift : Monad m => m ty -> Agent m ty evt
lift = Lift

export
step : a -> ((t : Nat) -> Agent m a evt) -> Agent m a evt
step = Step

export
noOp : Agent m () evt
noOp = NoOp

export
spawn : Agent m a evt -> Agent m () evt
spawn = Spawn

export
terminate : Agent m () evt
terminate = Terminate

export
time : Agent m Nat evt
time = Time

export
schedule : (event : evt) -> 
           (receiver : AgentId) -> 
           (t : Nat) -> 
           Agent m ty evt
schedule = Schedule

--export
--after : (td : Nat) -> Agent m a (t + td)
--after = After

-------------------------------------------------------------------------------
-- The obligatory ConsoleIO interface for easy debugging
--   only supports printing of strings, no input!
-------------------------------------------------------------------------------
public export
interface ConsoleIO (m : Type -> Type) where
  putStr : String -> Agent m () evt

export
ConsoleIO IO where
  putStr str = lift $ putStrLn str

export
putStrLn : ConsoleIO m => String -> Agent m () evt
putStrLn str = putStr (str ++ "\n")
-------------------------------------------------------------------------------