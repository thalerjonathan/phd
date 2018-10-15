module ABSMonadEventDriven

-- base
import Data.Vect
import Debug.Trace
-- contrib
import Data.SortedMap
-- project-local
import SortedQueueMap
import Random

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

-- NOTE: this is an event-driven approach to ABS
-- in another implementation we shall implement a
-- time-driven approach to ABS which can be 
-- achieved very similar but with encoding the 
-- time in the agent-monad

mutual
  export
  data Agent : (m : Type -> Type) -> 
               (ty : Type) ->
               (evt : Type) -> Type where
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

    ||| Draws a random boolean from uniform distribution
    RandomBool : (p : Double) -> Agent m Bool evt
    ||| Draws a random from exponential distribution
    RandomExp : (lambda : Double) -> Agent m Double evt
    ||| Draws a random element from a non-empty vector
    RandomElem : Vect (S n) elem -> Agent m elem evt

    -- TODO: the existence of an AgentId is not guaranteed and depends on time as agents
    -- can be created and terminated 
    ||| Schedules an event to be received by the receiver after t steps
    Schedule : (event : evt) -> (receiver : AgentId) -> (dt : DTime) -> Agent m () evt

    ||| Terminate the agent
    Terminate : Agent m () evt
    ||| Terminate the agent
    Spawn : AgentBehaviour m tyBeh evt -> Agent m AgentId evt

    ||| behave from now on like the new agent func
    Behaviour : (af : AgentBehaviour m tyBeh evt) -> Agent m () evt

  -- TODO: define reactive operations, build them on Schedule primitive
  --After : (td : Nat) -> Agent m a (t + td)
  --Occasionally : (td : Nat) -> Agent m a (t + td)

  public export
  Event : (evt : Type) -> Type
  Event evt = (AgentId, AgentId, evt) -- fst: sender, snd: receiver

  public export
  AgentBehaviour : (m : Type -> Type) -> 
                   (ty : Type) ->
                   (evt : Type) -> Type 
  AgentBehaviour m ty evt = (AgentId, evt) -> Agent m ty evt

export
Show (Agent m a evt) where
  show (Pure result) = "Pure"
  show (Bind x f) = "Bind"
  show (Lift x) = "Lift"
  show Terminate = "Terminate"
  show (Spawn ag) = "Spawn"
  show Now = "Now"
  show (Schedule _ _ _) = "Schedule"
  show (Behaviour _) = "Behaviour"
  show MyId = "MyId"
  show (RandomExp _) = "RandomExp"
  show (RandomBool _) = "RandomBool"
  show (RandomElem _) = "RandomElem"

EventQueue : (evt : Type) -> Type
EventQueue evt = SortedQueueMap Time (Event evt)

runAgentWithEventAux : Monad m =>
                      Agent m ty evt -> 
                      (aid : AgentId) ->
                      (t : Time) ->
                      (events : EventQueue evt) ->
                      (term : Bool) ->
                      (nextAid : AgentId) ->
                      (newBeh : Maybe (AgentBehaviour m tyBeh evt)) ->
                      (newAs : List (AgentId, AgentBehaviour m tyNew evt)) ->
                      (rs : RandomStream) -> 
                      m (ty, EventQueue evt, Bool, Maybe (AgentBehaviour m tyBeh evt), List (AgentId, AgentBehaviour m tyNew evt), AgentId, RandomStream)
runAgentWithEventAux (Pure result) aid t events term nextAid newBeh newAs rs
  = pure (result, events, term, newBeh, newAs, nextAid, rs)
runAgentWithEventAux (Bind act cont) aid t events term nextAid newBeh newAs rs = do
  (ret, events', term', newBeh', newAs', nextAid', rs') <- runAgentWithEventAux act aid t events term nextAid newBeh newAs rs
  runAgentWithEventAux (cont ret) aid t (merge events events') (term || term') nextAid' (newBeh <+> newBeh') (newAs ++ newAs') rs'
runAgentWithEventAux (Lift act) aid t events term nextAid newBeh newAs rs = do
  ret <- act
  pure (ret, events, term, newBeh, newAs, nextAid, rs)
runAgentWithEventAux Now aid t events term nextAid newBeh newAs rs
  = pure (t, events, term, newBeh, newAs, nextAid, rs)
runAgentWithEventAux MyId aid t events term nextAid newBeh newAs rs
  = pure (aid, events, term, newBeh, newAs, nextAid, rs)
runAgentWithEventAux (Schedule event receiver dt) aid t events term nextAid newBeh newAs rs = do
  let events' = insert (t + dt) (aid, receiver, event) events
  pure ((), events', term, newBeh, newAs, nextAid, rs)
runAgentWithEventAux Terminate aid t events term nextAid newBeh newAs rs
  = pure ((), events, True, newBeh, newAs, nextAid, rs)
runAgentWithEventAux (Spawn af) aid t events term nextAid newBeh newAs rs = do
  -- TODO: we are cheating here... can we solve it properly?
  -- TODO: would cong help here?
  let newAs' = ((nextAid, believe_me af)) :: newAs
  pure (nextAid, events, term, newBeh, newAs', (S nextAid), rs)
  --?runAgentWithEventAux_rhs_10
runAgentWithEventAux (Behaviour af) aid t events term nextAid newBeh newAs rs
  -- TODO: we are cheating here... can we solve it properly?
  -- TODO: would cong help here?
  = pure ((), events, term, Just (believe_me af), newAs, nextAid, rs)
  --= ?runAgentWithEventAux_rhs_11
runAgentWithEventAux (RandomBool p) aid t events term nextAid newBeh newAs rs = do
  let (rb, rs') = randomBool rs p
  pure (rb, events, term, newBeh, newAs, nextAid, rs')
runAgentWithEventAux (RandomExp lambda) aid t events term nextAid newBeh newAs rs = do
  let (re, rs') = randomExp rs lambda
  pure (re, events, term, newBeh, newAs, nextAid, rs')
runAgentWithEventAux (RandomElem xs) aid t events term nextAid newBeh newAs rs = do
  let (relem, rs') = randomElem rs xs
  pure (relem, events, term, newBeh, newAs, nextAid, rs')

-- TODO: when the size / type of a return-type is unsure, depends on the input
-- e.g. it is unknown how many new events the agent will schedule, then use
-- a dependent pair which holds the new size / type upon the return value 
-- depends: can use this in EventQueue when we are using a SortedQueueVectMap
-- QUESTION: can we extend this to general types as well? e.g. can we solve
-- this for AgentBehaviour ty'' and ty'?
runAgentWithEvent : Monad m =>
                    Agent m ty evt -> 
                    (aid : AgentId) ->
                    (t : Time) ->
                    (nextAid : AgentId) ->
                    (rs : RandomStream) -> 
                    m (ty, EventQueue evt, Bool, Maybe (AgentBehaviour m ty evt), List (AgentId, AgentBehaviour m ty evt), AgentId, RandomStream)
runAgentWithEvent a aid t nextAid rs = runAgentWithEventAux a aid t empty False nextAid Nothing [] rs

public export
data SimTermReason 
  = TimeLimit
  | EventLimit
  | NoEvents
  | NoAgents

public export
record SimulationResult where
  constructor MkSimulationResult
  --agents : (n' ** Vect n' (AgentId, AgentBehaviour m ty evt))
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
                  (as : SortedMap AgentId (AgentBehaviour m ty evt)) -> 
                  (nextAid : AgentId) ->
                  (aos : List (Time, AgentId, ty)) ->
                  (rs : RandomStream) ->
                  m SimulationResult
simulateUntilAux tLimit evtLimit t evtCnt events as nextAid aos rs = do
  if tLimit /= Z && t >= tLimit
    then pure $ MkSimulationResult t evtCnt TimeLimit aos
    else if evtLimit /= Z && evtCnt >= evtLimit
      then pure $ MkSimulationResult t evtCnt EventLimit aos
      else case first events of
        Nothing => pure $ MkSimulationResult t evtCnt NoEvents aos -- (_ ** as) -- no events => simulation terminates
        Just (evtTime, (sender, evtReceiver, evt)) => do
          let t' = evtTime
          case lookup evtReceiver as of
            Nothing => do -- receiver not found, ignore event
              let events' = dropFirst events
              simulateUntilAux tLimit evtLimit t' (S evtCnt) events' as nextAid aos rs
            Just af => do
              let a = af (sender, evt) -- to get the agent-monad apply the event to the agent-behaviour function
              (ret, newEvents, term, maf, newAs, nextAid', rs') <- runAgentWithEvent a evtReceiver t' nextAid rs
              let events' = merge (dropFirst events) newEvents
              let aos' = (t', evtReceiver, ret) :: aos

              case term of 
                -- terminate agent and ignore new behaviour
                True => do
                  let as' = delete evtReceiver as
                  if null as'
                    then pure $ MkSimulationResult t' evtCnt NoAgents aos -- (_ ** as) -- no agents => simulation terminates
                    else simulateUntilAux tLimit evtLimit t' (S evtCnt) events' as' nextAid' aos' rs'
                False => do
                  let as' = maybe as (\af => insert evtReceiver af as) maf
                  let as'' = insertFrom newAs as'
                  simulateUntilAux tLimit evtLimit t' (S evtCnt) events' as'' nextAid' aos' rs'

emptyObs : List (AgentId, ty)
emptyObs = []

export partial
simulateUntil : Monad m =>
                (tLimit : Time) ->
                (evtLimit : Nat) -> 
                (rngSeed : Integer) ->
                (initAgents : Vect n (AgentId, AgentBehaviour m ty evt)) ->
                (initEvents : Vect k (Time, Event evt)) -> 
                m SimulationResult
simulateUntil {ty} tLimit evtLimit rngSeed initAgents initEvents = do
    let events0 = fromVect initEvents
    let as0     = fromList $ toList initAgents
    let ks      = sortBy (\x, y => compare y x) $ keys as0
    let rs      = randoms rngSeed
    case head' ks of
      Nothing     => do
        -- let aos = the (List (AgentId, ty)) (cast emptyObs)
        --pure $ MkSimulationResult Z Z NoAgents aos --(believe_me emptyObs)
        -- TODO: fix this hole! I am desperate, I have no clue why the above
        -- cast is not working, also passing an empty list does not work as well
        -- TODO: would cong help here?
        ?should_never_happen_but_needs_to_be_fixed
      Just maxAid => simulateUntilAux tLimit evtLimit Z Z events0 as0 (S maxAid) [] rs

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
spawn : AgentBehaviour m ty evt -> Agent m AgentId evt
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
behaviour : (af : AgentBehaviour m tyBeh evt) -> 
            Agent m () evt
behaviour = Behaviour

export
randomBool : (p : Double) -> Agent m Bool evt
randomBool = RandomBool

export
randomExp : (lambda : Double) -> Agent m Double evt
randomExp = RandomExp

export
randomElem : Vect (S n) elem -> Agent m elem evt
randomElem = RandomElem

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