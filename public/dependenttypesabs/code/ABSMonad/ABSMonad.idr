module ABSMonad

-- base
import Data.Vect 
import Debug.Trace

%default total

export
-- TODO: add AgentId somehow
-- TODO: add time
-- TODO: its annoying that we need an 'a' for all operations, can we get rid of this?
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

  ||| Stepping operation: for every time-step run this function, need an a for returning observable data for current step
  Step : a -> ((t : Nat) -> Agent m a) -> Agent m a

  ||| Terminate the agent, need an a for returning observable data for current step
  Terminate : a -> Agent m a
  ||| Terminate the agent, need an a for returning observable data for current step
  Spawn : a -> Agent m a -> Agent m a

  NoOp : a -> Agent m a
  
  -- ||| Reactive operations 
  --After : (td : Nat) -> Agent m a (t + td)
  --Occasionally : (td : Nat) -> Agent m a (t + td)

export
Show (Agent m a) where
  show (Pure result) = "Pure"
  show (Bind x f) = "Bind"
  show (Lift x) = "Lift"
  show (Step x f) = "Step"
  show (Terminate a) = "Terminate"
  show (Spawn a ag) = "Spawn"
  show (NoOp a) = "NoOp"

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
runAgent aid (Step ret f) t term = do
  -- TODO should we really execute this in this time-step? seems to be wrong
  let t' = (S t)
  let ag = f t'
  pure (ret, ag, term)
runAgent aid (NoOp a) t term = do
  pure (a, (NoOp a), term)
runAgent aid ag@(Spawn a newAg) t term = do 
  -- TODO: add new agent
  pure (a, ag, term)
runAgent aid ag@(Terminate a) t term = do 
  -- TODO: terminate agent
  pure (a, ag, True)

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
step : a -> ((t : Nat) -> Agent m a) -> 
       Agent m a
step = Step

export
noOp : a -> Agent m a
noOp = NoOp

export
spawn : a -> Agent m a -> Agent m a
spawn = Spawn

export
terminate : a -> Agent m a
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



-- an agent is an indexed monad over 
-- m:       underlying computational (monad) context
-- ty:      return-type of the command
-- sa_pre:  some inner state s previous of the command
-- sa_post: some inner state s' after the command
-- evt:     an event/interaction protocoll
-- w, h:    2d environment boundary parameters
-- t:       current simulation time-step t
{-
export
data Agent : (m : Type -> Type) -> 
             (ty : Type) ->
             (sa_pre : Type) -> 
             (sa_post : ty -> Type) ->
             (evt : Type) ->
             (w : Nat) -> 
             (h : Nat) -> 
             (t : Nat) -> Type where
  ||| the monadic operations Pure and Bind for sequencing operations
  Pure : (result : ty) -> Agent m ty (out_fn sa_pre) out_fn evt w h t
  Bind : Agent m a sa_pre1 sa_post_fn1 evt w h t -> 
         ((result : a) -> Agent m b (sa_post_fn1 result) sa_post_fn2 evt w h t) ->
         Agent m b sa_pre1 sa_post_fn3 evt w h t

  ||| Lifting an action of the underlying computational contex into the agent monad
  Lift : Monad m => m ty -> Agent m ty sa_pre (const sa_post) evt w h t

  ||| Stepping operation: for every time-step run this function
  Step : ((t : Nat) -> Agent m a sa_pre sa_post_fn evt w h t) -> Agent m a sa_pre sa_post_fn evt w h t

  -- ||| Reactive operations 
  -- After 
  -- Occasionally

-- TODO: do we really need Monad m here?
export
runAgent : Monad m =>
           Agent m a sa_pre sa_postfn evt w h t -> 
           (t : Nat) ->
           m a
runAgent (Pure result) t = pure result
runAgent (Bind act cont) t = do
  ret <- runAgent act t
  runAgent (cont ret) t
runAgent (Lift act) t = do
  res <- act
  pure res
runAgent (Step f) t = do
  let ret = f t
  -- TODO: run all the other agents 
  runAgent ret (S t)

export
puree : (result : ty) -> 
       Agent m ty (out_fn sa_pre) out_fn evt w h t
puree = Pure

export
(>>=) : Agent m a sa_pre1 sa_post_fn1 evt w h t -> 
        ((result : a) -> Agent m b (sa_post_fn1 result) sa_post_fn2 evt w h t) ->
        Agent m b sa_pre1 sa_post_fn3 evt w h t
(>>=) = Bind

export
step : ((t : Nat) -> Agent m a sa_pre sa_post_fn evt w h t) -> 
       Agent m a sa_pre sa_post_fn evt w h t
step = Step
-}