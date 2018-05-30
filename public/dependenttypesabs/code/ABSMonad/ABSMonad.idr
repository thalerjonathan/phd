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
    Schedule : (event : evt) -> (receiver : AgentId) -> (td : Nat) -> Agent m () evt

    -- NOTE: doesnt work like this, nothing protects us from using Step multiple times within an agent, when the first one is happening,
    -- it will ignore all other step calls after
    -- ||| Stepping operation: for every time-step run this function, need an a for returning observable data for current step
    --Step : a -> ((t : Nat) -> Agent m a evt) -> Agent m a evt

    ||| Terminate the agent, need an a for returning observable data for current step
    Terminate : Agent m () evt
    ||| Terminate the agent, need an a for returning observable data for current step
    Spawn : AgentFunc m tyBeh evt -> Agent m () evt

    ||| Just an empty operation, for testing purposes, will be removed in final version
    NoOp : Agent m () evt

    ||| behave from now on like the new agent func
    Behaviour : (af : AgentFunc m tyBeh evt) -> Agent m AgentId evt

  -- TODO: define reactive operations, build them on Schedule primitive
  --After : (td : Nat) -> Agent m a (t + td)
  --Occasionally : (td : Nat) -> Agent m a (t + td)

  public export
  Event : (evt : Type) -> Type
  Event evt = (AgentId, AgentId, evt) -- fst: sender, snd: receiver

  public export
  AgentFunc : (m : Type -> Type) -> 
              (ty : Type) ->
              (evt : Type) -> Type 
  AgentFunc m ty evt = (AgentId, evt) -> Agent m ty evt

export
Show (Agent m a evt) where
  show (Pure result) = "Pure"
  show (Bind x f) = "Bind"
  show (Lift x) = "Lift"
  show Terminate = "Terminate"
  show (Spawn ag) = "Spawn"
  show NoOp = "NoOp"
  show Time = "Time"
  show (Schedule _ _ _) = "Schedule"
  show (Behaviour _) = "Behaviour"
  show MyId = "MyId"

EventQueue : (evt : Type) -> Type
EventQueue evt = SortedQueueMap Nat (Event evt)

runAgentWithEventAux : Monad m =>
                      Agent m ty evt -> 
                      (aid : AgentId) ->
                      (t : Nat) ->
                      (events : EventQueue evt) ->
                      (term : Bool) ->
                      (newBeh : Maybe (AgentFunc m tyBeh evt)) ->
                      (newAs : List (AgentId, AgentFunc m tyNew evt)) ->
                      m (ty, EventQueue evt, Bool, Maybe (AgentFunc m tyBeh evt), List (AgentId, AgentFunc m tyNew evt))
runAgentWithEventAux (Pure result) aid t events term newBeh newAs
  = pure (result, events, term, newBeh, newAs)
runAgentWithEventAux (Bind act cont) aid t events term newBeh newAs = do
  (ret, events', term', newBeh', newAs') <- runAgentWithEventAux act aid t events term newBeh newAs
  runAgentWithEventAux (cont ret) aid t (merge events events') (term || term') (newBeh <+> newBeh') (newAs ++ newAs')
runAgentWithEventAux (Lift act) aid t events term newBeh newAs = do
  ret <- act
  pure (ret, events, term, newBeh, newAs)
runAgentWithEventAux Time aid t events term newBeh newAs
  = pure (t, events, term, newBeh, newAs)
runAgentWithEventAux MyId aid t events term newBeh newAs
  = pure (aid, events, term, newBeh, newAs)
runAgentWithEventAux (Schedule event receiver td) aid t events term newBeh newAs = do
  let events' = insert (t + td) (aid, receiver, event) events
  pure ((), events', term, newBeh, newAs)
runAgentWithEventAux Terminate aid t events term newBeh newAs
  = pure ((), events, True, newBeh, newAs)
runAgentWithEventAux (Spawn af) aid t events term newBeh newAs = do
  -- TODO: we are cheating here... can we solve it properly?
  let newAs' = (believe_me af) :: newAs
  pure ((), events, term, newBeh, newAs')
  --?runAgentWithEventAux_rhs_10
runAgentWithEventAux NoOp aid t events term newBeh newAs
  = pure ((), events, term, newBeh, newAs)
runAgentWithEventAux (Behaviour af) aid t events term newBeh newAs
  -- TODO: we are cheating here... can we solve it properly?
  -- TODO: return a new agent-id
  = pure (aid, events, term, Just (believe_me af), newAs)
  --= ?runAgentWithEventAux_rhs_11

-- TODO: when the size / type of a return-type is unsure, depends on the input
-- e.g. it is unknown how many new events the agent will schedule, then use
-- a dependent pair which holds the new size / type upon the return value 
-- depends: can use this in EventQueue when we are using a SortedQueueVectMap
-- QUESTION: can we extend this to general types as well? e.g. can we solve
-- this for Agentfunc ty'' and ty'?
runAgentWithEvent : Monad m =>
                    Agent m ty evt -> 
                    (aid : AgentId) ->
                    (t : Nat) ->
                    m (ty, EventQueue evt, Bool, Maybe (AgentFunc m ty evt), List (AgentId, AgentFunc m ty evt))
runAgentWithEvent a aid t = runAgentWithEventAux a aid t empty False Nothing []

public export
data SimTermReason 
  = TimeLimit
  | EventLimit
  | NoEvents
  | NoAgents

export
record SimulationResult where
  constructor MkSimulationResult
  --agents : (n' ** Vect n' (AgentId, AgentFunc m ty evt))
  termTime     : Nat
  termEvtCount : Nat
  termReason   : SimTermReason

export
Show SimTermReason where
  show TimeLimit  = "TimeLimit"
  show EventLimit = "EventLimit"
  show NoEvents   = "NoEvents"
  show NoAgents   = "NoAgents"

export
Show SimulationResult where
  show (MkSimulationResult termTime termEvtCount termReason)
    = "Simulation terminated: " ++ show termReason ++ 
      "\n   termTime = " ++ show termTime ++ 
      "\n   termEvtCount = " ++ show termEvtCount

-- TODO: its not partial because 
--  EITHER we run out of events, which terminates the recursion
--  OR we will eventually hit the tLimit = Z case because
--     each event has a positive dt ?
-- WRONG: an agent could schedule events with dt = 0, which means
-- that we could be stuck in an infinte recursion when e.g. two
-- agents schedule events for each other with dt = 0 because 
-- time will never advance.
partial
simulateUntilAux : Monad m =>
                  (tLimit : Nat) ->
                  (evtLimit : Nat) ->
                  (t : Nat) ->
                  (evtCnt : Nat) ->
                  (events : EventQueue evt) -> 
                  (SortedMap AgentId (AgentFunc m ty evt)) -> 
                  m SimulationResult
simulateUntilAux tLimit evtLimit t evtCnt events as = do
  if t >= tLimit
    then pure $ MkSimulationResult t evtCnt TimeLimit
    else if evtCnt >= evtLimit
      then pure $ MkSimulationResult t evtCnt EventLimit
      else case first events of
        Nothing => pure $ MkSimulationResult t evtCnt NoEvents -- (_ ** as) -- no events => simulation terminates
        Just (evtTime, (sender, evtReceiver, evt)) => do
          case lookup evtReceiver as of
            Nothing => do -- receiver not found, ignore event
              let events' = dropFirst events
              simulateUntilAux tLimit evtLimit evtTime (S evtCnt) events' as
            Just af => do
              let a = af (sender, evt) -- to get the agent-monad apply the event to the agent-behaviour function
              (ret, newEvents, term, maf, newAs) <- runAgentWithEvent a evtReceiver evtTime
              let events' = merge (dropFirst events) newEvents

              -- TODO: handle observable value of the agent

              case term of 
                -- terminate agent and ignore new behaviour
                True => do
                  let as' = delete evtReceiver as
                  if null as'
                    then pure $ MkSimulationResult t evtCnt NoAgents -- (_ ** as) -- no agents => simulation terminates
                    else simulateUntilAux tLimit evtLimit evtTime (S evtCnt) events' as'
                False => do
                  let as' = maybe as (\af => insert evtReceiver af as) maf
                  let as'' = insertFrom newAs as'
                  simulateUntilAux tLimit evtLimit evtTime (S evtCnt) events' as'

export partial
simulateUntil : Monad m =>
                (tLimit : Nat) ->
                (evtLimit : Nat) -> 
                Vect n (AgentId, AgentFunc m tyBeh evt) ->
                Vect k (Nat, Event evt) -> 
                m SimulationResult
simulateUntil tLimit evtLimit as0 initEvents = 
  let evtQueue = fromVect initEvents
      asMap    = fromList $ toList as0
  in  simulateUntilAux tLimit evtLimit Z Z evtQueue asMap

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
           Agent m () evt
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


{-
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