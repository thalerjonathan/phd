module ABSMonad

-- base
import Data.Vect 
import Debug.Trace

%default total

export
-- TODO: add AgentId somehow
-- TODO: add time
data Agent : (m : Type -> Type) -> 
             (ty : Type) -> Type where
             --(t : Nat) -> Type where
  ||| the monadic operations Pure and Bind for sequencing operations
  Pure : (result : ty) -> Agent m ty
  Bind : Agent m a -> 
         ((result : a) -> Agent m b) ->
         Agent m b

  ||| Lifting an action of the underlying computational contex into the agent monad
  Lift : Monad m => m ty -> Agent m ty

  -- Schedule Event

  -- NOTE: doesnt work like this, nothing protects us from using Step multiple times within an agent, when the first one is happening,
  -- it will ignore all other step calls after
  -- ||| Stepping operation: for every time-step run this function, need an a for returning observable data for current step
  Step : a -> ((t : Nat) -> Agent m a) -> Agent m a

  ||| Terminate the agent, need an a for returning observable data for current step
  Terminate : Agent m ()
  ||| Terminate the agent, need an a for returning observable data for current step
  Spawn : Agent m a -> Agent m ()

  ||| Just an empty operation, for testing purposes, will be removed in final version
  NoOp : Agent m ()

-- ||| Reactive operations, built upon Schedule primitive
--After : (td : Nat) -> Agent m a (t + td)
--Occasionally : (td : Nat) -> Agent m a (t + td)

export
Show (Agent m a) where
  show (Pure result) = "Pure"
  show (Bind x f) = "Bind"
  show (Lift x) = "Lift"
  show (Step a f) = "Step"
  show Terminate = "Terminate"
  show (Spawn ag) = "Spawn"
  show NoOp = "NoOp"

public export
AgentId : Type 
AgentId = Nat

export
runAgent : Monad m =>
           AgentId ->
           Agent m a -> 
           (t : Nat) ->
           (term : Bool) ->
           m (a, Agent m a, Bool)
runAgent aid ag@(Pure res) t term = pure (res, ag, term)
runAgent aid ag@(Bind act cont) t term = do
  (ret, ag', term') <- runAgent aid act t term
  runAgent aid (cont ret) t (term || term')
runAgent aid ag@(Lift act) t term = do
  res <- act
  pure (res, ag, term)
runAgent aid (Step a f) t term = do
  -- TODO should we really execute this in this time-step? seems to be wrong
  let t' = (S t)
  let ag = f t'
  pure (a, ag, term)
runAgent aid NoOp t term 
  = pure ((), NoOp, term)
runAgent aid ag@(Spawn newAg) t term = do 
  -- TODO: add new agent
  pure ((), ag, term)
runAgent aid ag@(Terminate) t term 
  = pure ((), ag, True)

export
runAgents : Monad m =>
            (t : Nat) ->
            Vect n (AgentId, Agent m ty) ->
            m (n' ** Vect n' (AgentId, Agent m ty))
runAgents t [] = pure (_ ** [])
runAgents t ((aid, ag) :: as) = do
  (ret, ag', term) <- runAgent aid ag t False
  (n' ** as') <- runAgents t as
  case term of
    False => pure $ (_ ** (aid, ag') :: as')
    True  => pure $ (_ ** as')

export
runAgentsUntil : Monad m =>
                 (tLimit : Nat) ->
                 Vect n (AgentId, Agent m ty) ->
                 m (n' ** Vect n' (AgentId, Agent m ty))
runAgentsUntil tLimit as = runAgentsUntilAux tLimit Z as
  where
    runAgentsUntilAux : Monad m =>
                        (tLimit : Nat) ->
                        (t : Nat) ->
                        Vect n (AgentId, Agent m ty) ->
                        m (n' ** Vect n' (AgentId, Agent m ty))
    runAgentsUntilAux Z _ as = pure (_ ** as)
    runAgentsUntilAux (S tLimit) t as = do
      (n' ** as') <- runAgents t as
      -- no more agents left, simulation is over
      if n' == 0
        then pure (_ ** as')
        else runAgentsUntilAux tLimit (S t) as'

export
pure : (result : ty) -> 
       Agent m ty
pure = Pure

export
(>>=) : Agent m a -> 
        ((result : a) -> Agent m b) ->
        Agent m b
(>>=) = Bind

export
lift : Monad m => m ty -> Agent m ty
lift = Lift

export
step : a -> ((t : Nat) -> Agent m a) -> Agent m a
step = Step

export
noOp : Agent m ()
noOp = NoOp

export
spawn : Agent m a -> Agent m ()
spawn = Spawn

export
terminate : Agent m ()
terminate = Terminate

--export
--after : (td : Nat) -> Agent m a (t + td)
--after = After

-------------------------------------------------------------------------------
-- The obligatory ConsoleIO interface for easy debugging
--   only supports printing of strings, no input!
-------------------------------------------------------------------------------
public export
interface ConsoleIO (m : Type -> Type) where
  putStr : String -> Agent m ()

export
ConsoleIO IO where
  putStr str = lift $ putStrLn str

export
putStrLn : ConsoleIO m => String -> Agent m ()
putStrLn str = putStr (str ++ "\n")
-------------------------------------------------------------------------------