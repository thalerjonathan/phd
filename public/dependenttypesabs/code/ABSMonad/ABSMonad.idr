module ABSMonad

-- base
import Data.Vect
import Debug.Trace
-- contrib
import Data.SortedMap
-- project-local
import SortedQueueMap

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

    ||| gets the agents id
    MyId : Agent m AgentId evt

    -- TODO: the existence of an AgentId is not guaranteed and depends on time as agents
    -- can be created and terminated 
    ||| Schedules an event to be received by the receiver after t steps
    Schedule : (event : evt) -> (receiver : AgentId) -> (td : Nat) -> Agent m ty evt

    -- NOTE: doesnt work like this, nothing protects us from using Step multiple times within an agent, when the first one is happening,
    -- it will ignore all other step calls after
    -- ||| Stepping operation: for every time-step run this function, need an a for returning observable data for current step
    --Step : a -> ((t : Nat) -> Agent m a evt) -> Agent m a evt

    ||| Terminate the agent, need an a for returning observable data for current step
    Terminate : Agent m () evt
    ||| Terminate the agent, need an a for returning observable data for current step
    Spawn : AgentFunc m ty evt -> Agent m () evt

    ||| Just an empty operation, for testing purposes, will be removed in final version
    NoOp : Agent m () evt

    ||| behave from now on like the new agent func
    Behaviour : AgentFunc m ty evt -> Agent m () evt

  -- TODO: define reactive operations, build them on Schedule primitive
  --After : (td : Nat) -> Agent m a (t + td)
  --Occasionally : (td : Nat) -> Agent m a (t + td)

  export
  AgentFunc : (m : Type -> Type) -> 
              (ty : Type) ->
              (evt : Type) -> Type 
  AgentFunc m ty evt = evt -> Agent m ty evt

export
Show (Agent m a evt) where
  show (Pure result) = "Pure"
  show (Bind x f) = "Bind"
  show (Lift x) = "Lift"
  --show (Step a f) = "Step"
  show Terminate = "Terminate"
  show (Spawn ag) = "Spawn"
  show NoOp = "NoOp"
  show Time = "Time"
  show (Schedule _ _ _) = "Schedule"
  show (Behaviour _) = "Behaviour"
  show MyId = "MyId"

EventQueue : (evt : Type) -> Type
EventQueue evt = SortedQueueMap Nat (AgentId, evt)

{-
runAgent : Monad m =>
           AgentId ->
           Agent m a evt -> 
           (t : Nat) ->
           (term : Bool) ->
           (events : EventQueue n evt) ->
           m (a, Agent m a evt, Bool, EventQueue n evt)
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
runAgent aid MyId t term events 
  = pure (aid, MyId, term, events)

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
                        (events : EventQueue k evt) -> 
                        Vect n (AgentId, Agent m ty evt) ->
                        m (n' ** Vect n' (AgentId, Agent m ty evt))
    runAgentsUntilAux Z _ events as = pure (_ ** as)
    runAgentsUntilAux (S tLimit) t events as = do
      (n' ** as') <- runAgents t as
      -- no more agents left, simulation is over
      if n' == 0
        then pure (_ ** as')
        else runAgentsUntilAux tLimit (S t) events as' 
-}

runAgentWithEvent : Monad m =>
                    Agent m ty evt -> 
                    (aid : AgentId) ->
                    (t : Nat) ->
                    m (ty, EventQueue evt, Bool, Maybe (AgentFunc m ty' evt), List (AgentFunc m ty'' evt))
runAgentWithEvent a aid t = runAgentWithEventAux a empty False Nothing []
  where
    runAgentWithEventAux : Monad m =>
                           Agent m ty evt -> 
                           (events : EventQueue evt) ->
                           (term : Bool) ->
                           (newBeh : Maybe (AgentFunc m ty' evt)) ->
                           (newAs : List (AgentFunc m ty'' evt)) ->
                           m (ty, EventQueue evt, Bool, Maybe (AgentFunc m ty' evt), List (AgentFunc m ty'' evt))
    runAgentWithEventAux (Pure result) events term newBeh newAs
      -- TODO: need to proof that no events generated => n = n'?
      = pure (result, events, term, newBeh, newAs)
    runAgentWithEventAux (Bind act cont) events term newBeh newAs = do
      --(ret, events', term', newBeh', newAs') <- runAgentWithEventAux act events term newBeh newAs
      --runAgentWithEventAux (cont ret) (merge events events') (term || term') (newBeh <+> newBeh') (newAs ++ newAs')
      ?runAgentWithEventAux_rhs_1
    runAgentWithEventAux (Lift act) events term newBeh newAs = do
      ret <- act
      pure (ret, events, term, newBeh, newAs)
    runAgentWithEventAux Time events term newBeh newAs
      = pure (t, events, term, newBeh, newAs)
    runAgentWithEventAux MyId events term newBeh newAs
      = pure (aid, events, term, newBeh, newAs)
    runAgentWithEventAux (Schedule event receiver td) events term newBeh newAs = do
      ?runAgentWithEventAux_rhs_6
    runAgentWithEventAux Terminate events term newBeh newAs
      = pure ((), events, True, newBeh, newAs)
    runAgentWithEventAux (Spawn af) events term newBeh newAs 
      = ?runAgentWithEventAux_rhs_7 -- pure ((), events, term, newBeh, (af :: newAs))
    runAgentWithEventAux NoOp events term newBeh newAs
      = pure ((), events, term, newBeh, newAs)
    runAgentWithEventAux (Behaviour af) events term newBeh newAs
      = ?runAgentWithEventAux_rhs_11 -- pure ((), Just af, term) -- ?runAgentWithEventAux_rhs_11

Event : (evt : Type) -> Type
Event evt = (AgentId, evt)

record SimulationResult where
  constructor MkSimulationResult
  --agents : (n' ** Vect n' (AgentId, AgentFunc m ty evt))
  finalTime : Nat

simulateUntil : Monad m =>
                (tLimit : Nat) ->
                Vect n (AgentId, AgentFunc m ty evt) ->
                Vect k (Nat, Event evt) -> 
                m SimulationResult
simulateUntil tLimit as initEvents = 
    let evtQueue = fromVect initEvents
        asMap    = fromList $ toList as
    in  simulateUntilAux evtQueue Z asMap
  where
    simulateUntilAux : Monad m =>
                       (events : EventQueue evt) -> 
                       (t : Nat) ->
                       (SortedMap AgentId (AgentFunc m ty evt)) -> 
                       m SimulationResult 
    simulateUntilAux events t as = do
      case first events of
            Nothing => pure $ MkSimulationResult t -- (_ ** as) -- no events => simulation terminates
            Just (evtTime, (evtReceiver, evt)) => do
              case lookup evtReceiver as of
                   Nothing => do -- receiver not found, ignore event
                     -- TODO: remove element
                     --simulateUntilAux events evtTime as
                     ?simulateUntilAux_rhs1
                   Just af => do
                     let a = af evt -- to get the agent-monad apply the event to the agent-behaviour function
                     --(ret, maf, term) <- runAgentWithEvent a evtReceiver evtTime
                     ?simulateUntilAux_rhs1_3

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

{-
export
step : a -> ((t : Nat) -> Agent m a evt) -> Agent m a evt
step = Step
-}

export
noOp : Agent m () evt
noOp = NoOp

export
spawn : AgentFunc m ty evt -> Agent m () evt
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

export
myId : Agent m AgentId evt
myId = MyId

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