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
Time : Type
Time = Nat

public export
DTime : Type
DTime = Nat

public export
AgentId : Type 
AgentId = Nat

mutual
  export
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
    Now : Agent m Time evt

    ||| gets the agents id
    MyId : Agent m AgentId evt

    -- TODO: the existence of an AgentId is not guaranteed and depends on time as agents
    -- can be created and terminated 
    ||| Schedules an event to be received by the receiver after t steps
    Schedule : (event : evt) -> (receiver : AgentId) -> (dt : DTime) -> Agent m () evt

    ||| Terminate the agent, need an a for returning observable data for current step
    Terminate : Agent m () evt
    ||| Terminate the agent, need an a for returning observable data for current step
    Spawn : AgentFunc m tyBeh evt -> Agent m AgentId evt

    ||| Just an empty operation, for testing purposes, will be removed in final version
    NoOp : Agent m () evt

    ||| behave from now on like the new agent func
    Behaviour : (af : AgentFunc m tyBeh evt) -> Agent m () evt

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
  show Now = "Now"
  show (Schedule _ _ _) = "Schedule"
  show (Behaviour _) = "Behaviour"
  show MyId = "MyId"

EventQueue : (evt : Type) -> Type
EventQueue evt = SortedQueueMap Time (Event evt)

runAgentWithEventAux : Monad m =>
                      Agent m ty evt -> 
                      (aid : AgentId) ->
                      (t : Time) ->
                      (events : EventQueue evt) ->
                      (term : Bool) ->
                      (nextAid : AgentId) ->
                      (newBeh : Maybe (AgentFunc m tyBeh evt)) ->
                      (newAs : List (AgentId, AgentFunc m tyNew evt)) ->
                      m (ty, EventQueue evt, Bool, Maybe (AgentFunc m tyBeh evt), List (AgentId, AgentFunc m tyNew evt), AgentId)
runAgentWithEventAux (Pure result) aid t events term nextAid newBeh newAs
  = pure (result, events, term, newBeh, newAs, nextAid)
runAgentWithEventAux (Bind act cont) aid t events term nextAid newBeh newAs = do
  (ret, events', term', newBeh', newAs', nextAid') <- runAgentWithEventAux act aid t events term nextAid newBeh newAs
  runAgentWithEventAux (cont ret) aid t (merge events events') (term || term') nextAid' (newBeh <+> newBeh') (newAs ++ newAs')
runAgentWithEventAux (Lift act) aid t events term nextAid newBeh newAs = do
  ret <- act
  pure (ret, events, term, newBeh, newAs, nextAid)
runAgentWithEventAux Now aid t events term nextAid newBeh newAs
  = pure (t, events, term, newBeh, newAs, nextAid)
runAgentWithEventAux MyId aid t events term nextAid newBeh newAs
  = pure (aid, events, term, newBeh, newAs, nextAid)
runAgentWithEventAux (Schedule event receiver dt) aid t events term nextAid newBeh newAs = do
  let events' = insert (t + dt) (aid, receiver, event) events
  pure ((), events', term, newBeh, newAs, nextAid)
runAgentWithEventAux Terminate aid t events term nextAid newBeh newAs
  = pure ((), events, True, newBeh, newAs, nextAid)
runAgentWithEventAux (Spawn af) aid t events term nextAid newBeh newAs = do
  -- TODO: we are cheating here... can we solve it properly?
  let newAs' = ((nextAid, believe_me af)) :: newAs
  pure (nextAid, events, term, newBeh, newAs', (S nextAid))
  --?runAgentWithEventAux_rhs_10
runAgentWithEventAux NoOp aid t events term nextAid newBeh newAs
  = pure ((), events, term, newBeh, newAs, nextAid)
runAgentWithEventAux (Behaviour af) aid t events term nextAid newBeh newAs
  -- TODO: we are cheating here... can we solve it properly?
  = pure ((), events, term, Just (believe_me af), newAs, nextAid)
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
                    (t : Time) ->
                    (nextAid : AgentId) ->
                    m (ty, EventQueue evt, Bool, Maybe (AgentFunc m ty evt), List (AgentId, AgentFunc m ty evt), AgentId)
runAgentWithEvent a aid t nextAid = runAgentWithEventAux a aid t empty False nextAid Nothing []

public export
data SimTermReason 
  = TimeLimit
  | EventLimit
  | NoEvents
  | NoAgents

public export
record SimulationResult where
  constructor MkSimulationResult
  --agents : (n' ** Vect n' (AgentId, AgentFunc m ty evt))
  termTime     : Nat
  termEvtCount : Nat
  termReason   : SimTermReason
  agentOuts    : List (Time, AgentId, ty)

export
Show SimTermReason where
  show TimeLimit  = "TimeLimit"
  show EventLimit = "EventLimit"
  show NoEvents   = "NoEvents"
  show NoAgents   = "NoAgents"

export
Show SimulationResult where
  show (MkSimulationResult termTime termEvtCount termReason agentOuts)
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
                  (tLimit : Time) ->
                  (evtLimit : Nat) ->
                  (t : Time) ->
                  (evtCnt : Nat) ->
                  (events : EventQueue evt) -> 
                  (as : SortedMap AgentId (AgentFunc m ty evt)) -> 
                  (nextAid : AgentId) ->
                  (aos : List (Time, AgentId, ty)) ->
                  m SimulationResult
simulateUntilAux tLimit evtLimit t evtCnt events as nextAid aos = do
  if t >= tLimit
    then pure $ MkSimulationResult t evtCnt TimeLimit aos
    else if evtCnt >= evtLimit
      then pure $ MkSimulationResult t evtCnt EventLimit aos
      else case first events of
        Nothing => pure $ MkSimulationResult t evtCnt NoEvents aos -- (_ ** as) -- no events => simulation terminates
        Just (evtTime, (sender, evtReceiver, evt)) => do
          let t' = evtTime
          case lookup evtReceiver as of
            Nothing => do -- receiver not found, ignore event
              let events' = dropFirst events
              simulateUntilAux tLimit evtLimit t' (S evtCnt) events' as nextAid aos
            Just af => do
              let a = af (sender, evt) -- to get the agent-monad apply the event to the agent-behaviour function
              (ret, newEvents, term, maf, newAs, nextAid') <- runAgentWithEvent a evtReceiver t' nextAid
              let events' = merge (dropFirst events) newEvents
              let aos' = (t', evtReceiver, ret) :: aos

              case term of 
                -- terminate agent and ignore new behaviour
                True => do
                  let as' = delete evtReceiver as
                  if null as'
                    then pure $ MkSimulationResult t' evtCnt NoAgents aos -- (_ ** as) -- no agents => simulation terminates
                    else simulateUntilAux tLimit evtLimit t' (S evtCnt) events' as' nextAid' aos'
                False => do
                  let as' = maybe as (\af => insert evtReceiver af as) maf
                  let as'' = insertFrom newAs as'
                  simulateUntilAux tLimit evtLimit t' (S evtCnt) events' as'' nextAid' aos'

emptyObs : List (AgentId, ty)
emptyObs = []

export partial
simulateUntil : Monad m =>
                (tLimit : Time) ->
                (evtLimit : Nat) -> 
                (initAgents : Vect n (AgentId, AgentFunc m ty evt)) ->
                (initEvents : Vect k (Time, Event evt)) -> 
                m SimulationResult
simulateUntil {ty} tLimit evtLimit initAgents initEvents = do
    let events0 = fromVect initEvents
    let as0     = fromList $ toList initAgents
    let ks      = sortBy (\x, y => compare y x) $ keys as0
    case head' ks of
      Nothing     => do
        -- let aos = the (List (AgentId, ty)) (cast emptyObs)
        --pure $ MkSimulationResult Z Z NoAgents aos --(believe_me emptyObs)
        -- TODO: fix this hole! I am desperate, I have no clue why the above
        -- cast is not working, also passing an empty list does not work as well
        ?should_never_happen_but_needs_to_be_fixed
      Just maxAid => simulateUntilAux tLimit evtLimit Z Z events0 as0 (S maxAid) []

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
spawn : AgentFunc m ty evt -> Agent m AgentId evt
spawn = Spawn

export
terminate : Agent m () evt
terminate = Terminate

export
now : Agent m Time evt
now = Now

export
schedule : (event : evt) -> 
           (receiver : AgentId) -> 
           (dt : DTime) -> 
           Agent m () evt
schedule = Schedule

export
myId : Agent m AgentId evt
myId = MyId

export
behaviour : (af : AgentFunc m tyBeh evt) -> 
            Agent m () evt
behaviour = Behaviour

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