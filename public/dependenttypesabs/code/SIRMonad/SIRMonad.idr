module SIRMonad

-- base
import Data.Vect
-- local
import Random

%default total

infectivity : Double
infectivity = 0.05

contactRate : Double
contactRate = 5.0

illnessDuration : Double
illnessDuration = 15.0

Time : Type
Time = Nat

data SIRState
  = Susceptible
  | Infected
  | Recovered

Show SIRState where
  show Susceptible = "Susceptible"
  show Infected    = "Infected"
  show Recovered   = "Recovered"

||| An evidence of having made contact with another agent
-- TODO: this should be only valid for the current time-step
-- is opaque, so not possible to create
data SIRContact : Type where
  ContactWith : SIRState -> SIRContact

Show SIRContact where
  show (ContactWith s) = "ContactWith " ++ show s

-- TODO: add time to type and encode recovery after time
-- TODO: add environment boundaries to type and encode coordinates
data SIRAgent : (m : Type -> Type) -> 
                (ty : Type) ->
                SIRState -> 
                (ty -> SIRState) -> Type where

  -- BOILERPLATE operations
  ||| Monadic operations Pure and Bind for sequencing operations 
  Pure : (ret : ty) -> SIRAgent m ty pre postFn -- (postFn ret) postFn
  Bind : SIRAgent m a pre postFn1 -> 
          ((ret : a) -> SIRAgent m b (postFn1 ret) postFn2) ->
          SIRAgent m b pre postFn2
  ||| Lifting an action of the underlying computational contex into the agent monad
  Lift : Monad m => m ty -> SIRAgent m ty pre (const pre)

  -- RANDOM operations
  ||| Draws a random boolean from uniform distribution
  RandomBool : (p : Double) -> SIRAgent m Bool pre (const pre)
  ||| Draws a random from exponential distribution
  RandomExp : (lambda : Double) -> SIRAgent m Double pre (const pre)
  ||| Draws a random element from a non-empty vector
  RandomElem : Vect (S n) elem -> SIRAgent m elem pre (const pre)

  -- TIME operations
  Now : SIRAgent m Time pre (const pre)

  -- DOMAIN SPECIFIC operations
  ||| Making contact with another agent. Note there will be always another agent, because there is always at least one agent: self
  MakeContact : SIRAgent m SIRContact Susceptible (const Susceptible)

pure : (ret : ty) -> SIRAgent m ty pre postFn --(postFn ret) postFn
pure = Pure

(>>=) : SIRAgent m a pre postFn1 -> 
        ((ret : a) -> SIRAgent m b (postFn1 ret) postFn2) ->
        SIRAgent m b pre postFn2
(>>=) = Bind

lift : Monad m => m ty -> SIRAgent m ty pre (const pre)
lift = Lift

randomBool : (p : Double) -> SIRAgent m Bool pre (const pre)
randomBool = RandomBool

randomExp : (lambda : Double) -> SIRAgent m Double pre (const pre)
randomExp = RandomExp

randomElem : Vect (S n) elem -> SIRAgent m elem pre (const pre)
randomElem = RandomElem

now : SIRAgent m Time pre (const pre)
now = Now

makeContact : SIRAgent m SIRContact Susceptible (const Susceptible)
makeContact = MakeContact

runSIRAgent : Monad m => 
              SIRAgent m ty pre postFn -> 
              (t : Time) ->
              (rs : RandomStream) ->
              m (ty, RandomStream)
runSIRAgent (Pure ret) t rs
  = pure (ret, rs)
runSIRAgent (Bind act cont) t rs = do
  (ret', rs') <- runSIRAgent act t rs
  runSIRAgent (cont ret') t rs'
runSIRAgent (Lift act) t rs = do
  ret <- act
  pure (ret, rs)
runSIRAgent (RandomBool p) t rs = do
  let (r, rs') = randomBool rs p
  pure (r, rs')
runSIRAgent (RandomExp lambda) t rs = do
  let (r, rs') = randomExp rs lambda
  pure (r, rs')
runSIRAgent (RandomElem xs) t rs = do
  let (r, rs') = randomElem rs xs
  pure (r, rs')
runSIRAgent MakeContact t rs = do
  -- TODO: pick randomly from all agents
  -- NOTE: for now 20% chance
  let (r, rs') = randomBool rs 0.2
  case r of
    True  => pure (ContactWith Infected, rs')
    False => pure (ContactWith Susceptible, rs')
runSIRAgent Now t rs
  = pure (t, rs)

runAgents : Monad m => 
            List (SIRAgent m ty pre postFn) -> 
            (t : Time) ->
            (rs : RandomStream) ->
            m (List (SIRAgent m ty pre postFn), RandomStream)
runAgents [] t rs = pure ([], rs)
runAgents (a :: as) t rs = do
  (ret, rs') <- runSIRAgent a t rs

  (as', rs') <- runAgents as t rs
  pure (a :: as', rs')

runSimulationAux : Monad m => 
                   (steps : Nat) ->
                   (t : Nat) ->
                   List (SIRAgent m ty pre postFn) -> 
                   (rs : RandomStream) ->
                   m ()
runSimulationAux Z t as rs = pure ()
runSimulationAux (S k) t as rs = do
  (as', rs') <- runAgents as t rs
  runSimulationAux k (S t) as' rs'

runSimulation : Monad m => 
                (steps : Nat) ->
                List (SIRAgent m ty pre postFn) -> 
                (rs : RandomStream) ->
                m ()
runSimulation steps as rs = runSimulationAux steps Z as rs

-------------------------------------------------------------------------------
-- The obligatory ConsoleIO interface for easy debugging
--   only supports printing of strings, no input!
-------------------------------------------------------------------------------
interface ConsoleIO (m : Type -> Type) where
  putStr : String -> SIRAgent m () pre (const pre)

ConsoleIO IO where
  putStr str = lift $ putStrLn str

putStrLn : ConsoleIO m => String -> SIRAgent m () pre (const pre)
putStrLn str = putStr (str ++ "\n")
-------------------------------------------------------------------------------

recovered : ConsoleIO m =>
            SIRAgent m Bool Recovered (const Recovered)
recovered = do
  putStrLn "I stay recovered"
  pure True

infected : ConsoleIO m =>
           (recTime : Nat) ->
           SIRAgent m Bool Infected (\rec => case rec of 
                                                  True  => Recovered
                                                  False => Infected)
infected recTime = do
  t <- now
  putStrLn $ "t = " ++ show t
  putStrLn $ "will recover at " ++ show recTime
  -- pure (recTime >= t)
  case (recTime > t) of
    True  => do
      putStrLn "still infected..."
      pure True
    False => do
      putStrLn "recovered!"
      pure True
  

contact : ConsoleIO m =>
          Nat -> 
          SIRAgent m Bool Susceptible (\inf => case inf of 
                                                  True  => Infected
                                                  False => Susceptible)
contact Z     = pure False
contact (S k) = do
  c <- makeContact
  putStrLn $ show c

  case c of
    ContactWith Infected => do
      inf <- randomBool infectivity
      putStrLn $ "infected: " ++ show inf
    
      case inf of
        True  => pure True
        False => do
          ret <- contact k
          pure ret
    ContactWith _ => do
      ret <- contact k
      pure ret

susceptible : ConsoleIO m =>
              SIRAgent m Bool Susceptible (\inf => case inf of 
                                                      True  => Infected
                                                      False => Susceptible)
susceptible = do
  r <- randomExp (1 / contactRate)
  putStrLn $ "r = " ++ show r

  let numCont = fromIntegerNat $ cast r
  putStrLn $ "numCont = " ++ show numCont
  
  ret <- contact numCont
  case ret of
    True => do
      putStrLn $ "got infected"
      pure True
    False => do
      putStrLn $ "stay susceptible"
      pure False -- this is certainly wrong?

runSIR : IO ()
runSIR = do
  let rs = randoms 1
  
  --(ret, rs') <- runSIRAgent (susceptible) Z rs
  --putStrLn $ "terminated with return value " ++ show ret

  --runAgents [infected 5] 10 rs

  runSimulation 10 [infected 5] rs

  

{-
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
  let newAs' = ((nextAid, believe_me af)) :: newAs
  pure (nextAid, events, term, newBeh, newAs', (S nextAid), rs)
  --?runAgentWithEventAux_rhs_10
runAgentWithEventAux (Behaviour af) aid t events term nextAid newBeh newAs rs
  -- TODO: we are cheating here... can we solve it properly?
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
                (rngSeed : Int) ->
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
-}