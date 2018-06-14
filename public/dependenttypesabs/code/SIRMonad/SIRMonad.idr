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

-- TODO: add time to type
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
  ||| returns the current simulation time 
  -- TODO: do we really need it when we encode it already in types?
  -- TODO: alternative: 
  -- Now : SIRAgent m t t pre (const pre)
  Now : SIRAgent m Time pre (const pre)

  -- DOMAIN SPECIFIC operations
  ||| Making contact with another agent. Note there will be always another agent, because there is always at least one agent: self
  MakeContact : SIRAgent m SIRContact Susceptible (const Susceptible)

  -- TODO: add operations for transitions 
  -- Susceptible to Infected: 
  -- Infected to Recovered: Recover
  --Recover : (at : Time) -> SIRAgent m () t  Infected (\_ => case t > at of
  --                                                             True  => Recovered
  --                                                              False => Infected)

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

{-
recover : (at : Time) -> SIRAgent m () Infected (\_ => case t > at of
                                                                True  => Recovered
                                                                False => Infected)
recover = Recover
-}

runSIRAgent : Monad m => 
              SIRAgent m ty pre postFn -> 
              (time : Time) ->
              (rs : RandomStream) ->
              m (ty, RandomStream)
runSIRAgent (Pure ret) time rs
  = pure (ret, rs)
runSIRAgent (Bind act cont) time rs = do
  (ret', rs') <- runSIRAgent act time rs
  runSIRAgent (cont ret') time rs'
runSIRAgent (Lift act) time rs = do
  ret <- act
  pure (ret, rs)
runSIRAgent (RandomBool p) time rs = do
  let (r, rs') = randomBool rs p
  pure (r, rs')
runSIRAgent (RandomExp lambda) time rs = do
  let (r, rs') = randomExp rs lambda
  pure (r, rs')
runSIRAgent (RandomElem xs) time rs = do
  let (r, rs') = randomElem rs xs
  pure (r, rs')
runSIRAgent Now time rs
  = pure (time, rs)
runSIRAgent MakeContact time rs = do
  -- TODO: pick randomly from all agents
  -- NOTE: for now 20% chance
  let (r, rs') = randomBool rs 0.2
  case r of
    True  => pure (ContactWith Infected, rs')
    False => pure (ContactWith Susceptible, rs')
--runSIRAgent (Recover at) time rs 
--  = pure ((), rs) --?runSIRAgent_rhs2

runAgents : Monad m => 
            List (SIRAgent m ty pre postFn) -> 
            (time : Time) ->
            (rs : RandomStream) ->
            m (List (SIRAgent m ty pre postFn), RandomStream)
runAgents [] time rs = pure ([], rs)
runAgents (a :: as) time rs = do
  (ret, rs') <- runSIRAgent a time rs

  (as', rs') <- runAgents as time rs
  pure (a :: as', rs')

runSimulationAux : Monad m => 
                   (steps : Nat) ->
                   (time : Time) ->
                   List (SIRAgent m ty pre postFn) -> 
                   (rs : RandomStream) ->
                   m ()
runSimulationAux Z time as rs = pure ()
runSimulationAux (S k) time as rs = do
  (as', rs') <- runAgents as time rs
  runSimulationAux k (S time) as' rs'

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

{-
infected' : ConsoleIO m =>
           (recTime : Nat) ->
           SIRAgent m Bool t Infected (\_ => case t > recTime of 
                                                  True  => Recovered
                                                  False => Infected)
infected' recTime = pure True
-}

infected : ConsoleIO m =>
           (recTime : Nat) ->
           SIRAgent m Bool Infected (\rec => case rec of 
                                                  True  => Recovered
                                                  False => Infected)
infected recTime = do
  time <- now
  putStrLn $ "time = " ++ show time
  putStrLn $ "will recover at " ++ show recTime
  case (time > recTime) of
    True  => do
      putStrLn "recovered!"
      pure True
    False => do
      putStrLn "still infected..."
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
      pure False

runSIR : IO ()
runSIR = do
  let rs = randoms 1
  
  --(ret, rs') <- runSIRAgent ag Z rs
  --putStrLn $ "terminated with return value " ++ show ret

  --runAgents [infected 5] 10 rs
  --runSimulation' 10 [susceptible] rs --[infected 5] rs

  --putStrLn "Fuck off"

  pure ()