module ABSMonad

-- base
import Data.Vect 
-- contrib
import Data.SortedMap

%default total

export
data Agent : (m : Type -> Type) -> 
             (ty : Type) ->
             (t : Nat) -> Type where
  ||| the monadic operations Pure and Bind for sequencing operations
  Pure : (result : ty) -> Agent m ty t
  Bind : Agent m a t -> 
         ((result : a) -> Agent m b t) ->
         Agent m b t

  ||| Lifting an action of the underlying computational contex into the agent monad
  Lift : Monad m => m ty -> Agent m ty t

  ||| Stepping operation: for every time-step run this function
  Step : ((t : Nat) -> Agent m a t) -> Agent m a t

  -- ||| Reactive operations 
  --After : (td : Nat) -> Agent m a (t + td)
  --Occasionally : (td : Nat) -> Agent m a (t + td)

public export
AgentId : Type 
AgentId = Nat

{-
export
runAgentsUntil : Monad m =>
                 (tLimit : Nat) ->
                 Vect n (AgentId, Agent m ty t) -> 
                 Vect tLimit ty
runAgentsUntil tLimit as = 
    let am = insertFrom as empty
    in  runAgentsUntilAux tLimit am
  where
    runAgentsUntilAux : Monad m =>
                        (tLimit : Nat) ->
                        SortedMap AgentId (Agent m ty t) -> 
                        Vect tLimit ty
    runAgentsUntilAux Z am = []
    runAgentsUntilAux (S k) am = 
      let aml = toList am

      in  ?runAgentsUntilAux_rhs_2
-}

runAgents : Monad m =>
            (t : Nat) ->
            Vect n (AgentId, Agent m ty t) ->
            Vect n (AgentId, Agent m ty t)
runAgents t [] = []
runAgents t ((aid, a) :: as) = 
  case a of 
    (Pure result) => (aid, a) :: runAgents t as
    (Bind act cont) => (aid, a) :: runAgents t as
    (Lift act) => (aid, a) :: runAgents t as -- do
      --res <- act
      --pure $ (aid, a) :: runAgents as
    (Step f) => 
      let a' = f t 
      in  (aid, a') :: runAgents t as

export
runAgentsUntilAux : Monad m =>
                    (tLimit : Nat) -> 
                    (t : Nat) ->
                    Vect n (AgentId, Agent m ty t) -> 
                    Vect n (AgentId, Agent m ty t')-- (tLimit + t))
runAgentsUntilAux Z t as = as
runAgentsUntilAux (S k) t as = 
  let as' = runAgents t as
--      as'' = runAgentsUntilAux k t as'
  in  ?runAgentsUntilAux_rhs
{-
runAgentsUntil : Monad m =>
                (tLimit : Nat) -> 
                Vect n (AgentId, Agent m ty t) -> 
                Vect n (AgentId, Agent m ty (tLimit + t))
runAgentsUntil tLimit as = runAgentsUntilAux tLimit Z as
    --let as' = runAgents t as
    --in  ?runAgentsUntil_rhs -- runAgentsUntil k (S t) as'
  where
    runAgentsUntilAux : Monad m =>
                       (tLimit : Nat) -> 
                       (t : Nat) ->
                       Vect n (AgentId, Agent m ty t) -> 
                       Vect n (AgentId, Agent m ty (tLimit + t))
    runAgentsUntilAux Z t as = as
    runAgentsUntilAux (S k) t as = ?runAgentsUntil_rhs
-}

-- TODO: do we really need Monad m here?
export
runAgent : Monad m =>
           AgentId ->
           Agent m a t -> 
           (t : Nat) ->
           m a
runAgent aid (Pure res) t = pure res
runAgent aid (Bind act cont) t = do
  ret <- runAgent aid act t
  runAgent aid (cont ret) t
runAgent aid (Lift act) t = do
  res <- act
  pure res
runAgent aid (Step f) t = do
  let t' = (S t)
  let ret = f t'
  runAgent aid ret t'

export
pure : (result : ty) -> 
       Agent m ty t
pure = Pure

export
(>>=) : Agent m a t -> 
        ((result : a) -> Agent m b t) ->
        Agent m b t
(>>=) = Bind

export
lift : Monad m => m ty -> Agent m ty t
lift = Lift

export
step : ((t : Nat) -> Agent m a t) -> 
       Agent m a t
step = Step

--export
--after : (td : Nat) -> Agent m a (t + td)
--after = After

-------------------------------------------------------------------------------
-- The obligatory ConsoleIO interface for easy debugging
--   only supports printing of strings, no input!
-------------------------------------------------------------------------------
public export
interface ConsoleIO (m : Type -> Type) where
  putStr : String -> Agent m () t

export
ConsoleIO IO where
  putStr str = lift $ putStrLn str

export
putStrLn : ConsoleIO m => String -> Agent m () t
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