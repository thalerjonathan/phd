module SIRAgent

import Control.Monad.State
import Data.Vect
import Effects
import Effect.Random

import Debug.Trace

import Export
import Random
import SIRState

%default total

contactRate : Double
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

||| The data of an initialised SIR agent
-- TODO: put other data here e.g. coordinates
data SIRAgent : SIRState -> Type where
  MkSIRAgent : Nat -> SIRAgent s

||| An evidence of having made contact with another agent
-- TODO: this should be only valid for the current time-step
-- is opaque, so not possible to create
data MakeContact : Type where
  ContactWith : SIRState -> MakeContact

calcInfectType : Bool -> Type
calcInfectType True  = SIRAgent Infected
calcInfectType False = SIRAgent Susceptible

calcRecoverType : Bool -> Type
calcRecoverType True  = SIRAgent Recovered
calcRecoverType False = SIRAgent Infected

data Sir : Effect where
  --Init : (s : SIRState) -> Sir AgentId () (const $ SIRAgent s)
  MakeRandomContact : Sir MakeContact (SIRAgent Susceptible) (const $ SIRAgent Susceptible)
  Infect : MakeContact -> Sir Bool 
                          (SIRAgent Susceptible) 
                          (\res => calcInfectType res) -- TODO: why cant we simply use calcInfectType ?
  -- TODO: can we encode recovery over time?
  Recover : Sir Bool 
            (SIRAgent Infected) 
            (\res => calcRecoverType res) -- TODO: why cant we simply use calcRecoverType ?

  RandomExp : Double -> Sir Double (SIRAgent s) (const $ SIRAgent s)

  -- TODO: implement: passing a continuation as argument
  -- this will give the control back to the system for the next time-step
  -- NOTE: this won't work, continuations do not work with Eff
  --Step : ((t : Nat) -> Sir () (SIRAgent s) (const $ SIRAgent s)) 
  --       -> Sir () (SIRAgent s) (const $ SIRAgent s)

SIR : Type -> EFFECT
SIR t = MkEff t Sir

{-
init :  (s : SIRState) 
     -> Eff AgentId
        [SIR ()] 
        [SIR (SIRAgent s)]
init s = call $ Init s
-}

makeRandomContact : Eff MakeContact 
                      [SIR (SIRAgent Susceptible)]
makeRandomContact = call MakeRandomContact

infect : MakeContact -> 
         Eff Bool
          [SIR (SIRAgent Susceptible)] 
          (\res => [SIR (calcInfectType res)])
infect c = call $ Infect c

recover : Eff Bool
            [SIR (SIRAgent Infected)] 
            (\res => [SIR (calcRecoverType res)])
recover = call Recover 

randomExp : Double ->
            Eff Double
              [SIR (SIRAgent s)] 
randomExp lambda = call $ RandomExp lambda
-------------------------------------------------------------------------------
-- State Handler
-------------------------------------------------------------------------------
record SimulationState where
  constructor MkSimulationState
  time     : Nat
  susCount : Nat
  infCount : Nat
  recCount : Nat
  rng      : RandomStream

Show SimulationState where
  show (MkSimulationState time s i r _) = "SimulationState t = " ++ show time ++ " s = " ++ show s ++ " i = " ++ show i ++ " r = " ++ show r

mkSimulationState : Int -> SimulationState
mkSimulationState seed = 
  let rng = randoms seed
  in  MkSimulationState Z Z Z Z rng

randomExpState : Double -> StateT SimulationState Identity Double
randomExpState lambda = do
  (MkSimulationState t s i r rng) <- get
  let (e, rng') = Random.randomExp rng lambda
  put (MkSimulationState t s i r rng')
  pure e

randomBoolState : Double -> StateT SimulationState Identity Bool
randomBoolState p = do
  (MkSimulationState t s i r rng) <- get
  let (b, rng') = Random.randomBool rng p
  put (MkSimulationState t s i r rng')
  pure b

Handler Sir (StateT SimulationState Identity) where
  {-
    handle () (Init s) k = do
      case s of 
        Susceptible => do
          k Z (MkSIRAgent Z)
        Infected    => do
          e <- randomExpState (1 / illnessDuration)
          k Z (MkSIRAgent (fromIntegerNat $ cast e)) 
        Recovered   => do
          k Z (MkSIRAgent Z)
          -}
  handle r MakeRandomContact k = do
    (MkSimulationState _ s i _ _) <- get
    let infFract = cast {to=Double} i / cast {to=Double} s
    inf <- randomBoolState (infFract * infectivity)

    case inf of
      True  => k (ContactWith Infected) r
      False => k (ContactWith Susceptible) r -- this is actually not completely accurate, contact can also happen with recovered but the effect is the same => no infection will happen with either one

  handle r (Infect (ContactWith s)) k = 
    case s of 
      Susceptible => do
        k False r
      Infected    => do
        (MkSimulationState t _ _ _ _) <- get
        e <- randomExpState (1 / illnessDuration)
        k True (MkSIRAgent (t + (fromIntegerNat $ cast e)))
      Recovered   => do
        k False r
  handle (MkSIRAgent t') Recover k = do
    (MkSimulationState t _ _ _ _) <- get
    case compare t t' of
      EQ  => k True (MkSIRAgent Z)
      GT  => k True (MkSIRAgent Z)
      _   => k False (MkSIRAgent t')
  handle r (RandomExp lambda) k = do
    e <- randomExpState lambda
    k e r
  --handle r (Step cont) k = do
  --  -- TODO: implement - put continuation into map of agents
  --  ?handle_rhs

----------------------------------------------------------------------------
-- Model implementation
-----------------------------------------------------------------------------
susceptible : Eff Bool
                [SIR (SIRAgent Susceptible)] 
                (\res => [SIR (calcInfectType res)])
susceptible = do
    n <- randomExp (1 / contactRate)
    contact (fromIntegerNat $ cast n)
  where
    contact : Nat ->
              Eff (Bool)
                [SIR (SIRAgent Susceptible)] 
                (\res => [SIR (calcInfectType res)])
    contact Z = pureM False
    contact (S k) = do
      c <- makeRandomContact
      i <- infect c
      case i of 
        True  => pureM True
        False => contact k

infected : Eff Bool
            [SIR (SIRAgent Infected)] 
            (\res => [SIR (calcRecoverType res)])
infected = recover
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- running the Simulation
-----------------------------------------------------------------------------
partial
runSIR : (s : Nat) ->
         (i : Nat) ->
         (r : Nat) ->
         List (Nat, Nat, Nat)
runSIR s i r = 
    let simState0 = mkSimulationState 42
        sus = createAs s Susceptible
        (inf, simState') = runState (createInfs i) simState0
        rec = createAs r Recovered
        (dyns, simStateFinal) = runState (runAllAgents sus inf rec []) simState'
    in  dyns
  where
    runSusceptibles : List (SIRAgent Susceptible) ->
                      StateT SimulationState Identity (List (SIRAgent Susceptible), List (SIRAgent Infected))
    runSusceptibles [] = pure ([], [])
    runSusceptibles (a :: sus) = do
      (ret ** (a' :: _)) <- runEnv [a] susceptible
      (sus', infNew) <- runSusceptibles sus
      case ret of
        True  => pure (sus', a' :: infNew)
        False => pure (a' :: sus', infNew)

    runInfected : List (SIRAgent Infected) ->
                  StateT SimulationState Identity (List (SIRAgent Infected), List (SIRAgent Recovered))
    runInfected [] = pure ([], [])
    runInfected (a :: inf) = do
      (ret ** (a' :: _)) <- runEnv [a] infected
      (inf', recNew) <- runInfected inf
      case ret of
        True  => pure (inf', a' :: recNew)
        False => pure (a' :: inf', recNew)

    partial
    runAllAgents : List (SIRAgent Susceptible) ->
                   List (SIRAgent Infected) ->
                   List (SIRAgent Recovered) ->
                   List (Nat, Nat, Nat) -> 
                   StateT SimulationState Identity (List (Nat, Nat, Nat))
    runAllAgents sus [] rec acc = pure (reverse acc)
    runAllAgents sus inf rec acc = do
      (sus', infNew) <- runSusceptibles sus 
      (inf', recNew) <- runInfected inf 
      -- no need to run recovered

      let susNext = sus'
      let infNext = inf' ++ infNew
      let recNext = rec ++ recNew

      let acc' = (length susNext, length infNext, length recNext) :: acc

      (MkSimulationState t s i r rng) <- get
      put (MkSimulationState (S t) s i r rng)

      trace ("t = " ++ show t) (runAllAgents susNext infNext recNext acc')

    createAs : (n : Nat) -> (s : SIRState) -> List (SIRAgent s)
    createAs Z s = []
    createAs (S k) s = MkSIRAgent Z :: createAs k s

    createInfs : (n : Nat) -> StateT SimulationState Identity (List (SIRAgent Infected))
    createInfs Z = pure []
    createInfs (S k) = do
      as <- createInfs k
      e <- randomExpState (1 / illnessDuration)
      pure (MkSIRAgent (fromIntegerNat $ cast e) :: as)

namespace Main
  partial
  main : IO ()
  main = do
    let ss = runSIR 9 1 0
    
    writeMatlabFile "sir_dep_eff.m" ss
    --putStrLn $ show ss
-----------------------------------------------------------------------------