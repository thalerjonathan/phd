module SIRAgent

import Control.Monad.State
import Data.Vect
import Effects
import Effect.Random

import SIRState

%default total

contactRate : Double
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

AgentId : Type
AgentId = Nat

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
  Init : (s : SIRState) -> Sir AgentId () (const $ SIRAgent s)
  MakeRandomContact : Sir MakeContact (SIRAgent Susceptible) (const $ SIRAgent Susceptible)
  Infect : MakeContact -> Sir Bool 
                          (SIRAgent Susceptible) 
                          (\res => calcInfectType res) -- TODO: why cant we simply use calcInfectType ?
  -- TODO: can we encode recovery over time?
  Recover : Sir Bool 
            (SIRAgent Infected) 
            (\res => calcRecoverType res) -- TODO: why cant we simply use calcRecoverType ?
  -- TODO: remove this, SIR agents never die
  Kill : Sir () (SIRAgent s) (\res => ())

  -- TODO: implement: passing a continuation as argument
  -- this will give the control back to the system for the next time-step
  -- NOTE: this won't work, continuations do not work with Eff
  --Step : ((t : Nat) -> Sir () (SIRAgent s) (const $ SIRAgent s)) 
  --       -> Sir () (SIRAgent s) (const $ SIRAgent s)

SIR : Type -> EFFECT
SIR t = MkEff t Sir

init :  (s : SIRState) 
     -> Eff AgentId
        [SIR ()] 
        [SIR (SIRAgent s)]
init s = call $ Init s

makeRandomContact : Eff MakeContact 
                    [SIR (SIRAgent Susceptible)]
makeRandomContact = call MakeRandomContact

infect :  MakeContact
       -> Eff (Bool) 
          [SIR (SIRAgent Susceptible)] 
          (\res => [SIR (calcInfectType res)])
infect c = call $ Infect c

recover : Eff (Bool) 
          [SIR (SIRAgent Infected)] 
          (\res => [SIR (calcRecoverType res)])
recover = call Recover 

kill : Eff () 
       [SIR (SIRAgent s)]
       [SIR ()]
kill = call Kill

-------------------------------------------------------------------------------
-- State Handler
-------------------------------------------------------------------------------
record SimulationState where
  constructor MkSimulationState
  time        : Nat
  nextAgentId : AgentId

Show SimulationState where
  show (MkSimulationState time _) = "SimulationState t = " ++ show time

mkSimulationState : SimulationState
mkSimulationState = MkSimulationState Z Z

Handler Sir (Control.Monad.State.StateT SimulationState Identity) where
  handle () (Init s) k = do
    case s of 
      Susceptible => do
        k Z (MkSIRAgent Z)
      Infected    => do
        k Z (MkSIRAgent (fromIntegerNat $ cast illnessDuration)) -- TODO: randomExp (1 / illnessDuration)
      Recovered   => do
        k Z (MkSIRAgent Z)
  handle r MakeRandomContact k = do
    -- TODO: pick random other agent
    let con = ContactWith Infected
    k con r
  handle r (Infect (ContactWith s)) k = 
    case s of 
      Susceptible => do
        k False r
      Infected    => do
        k True (MkSIRAgent (fromIntegerNat $ cast illnessDuration)) -- TODO: randomExp (1 / illnessDuration)
      Recovered   => do
        k False r
  handle (MkSIRAgent dur) Recover k = do
    case minus dur 1 of
      Z        => k True (MkSIRAgent Z)
      (S dur') => k False (MkSIRAgent dur')
  handle r Kill k = do
    k () ()
  --handle r (Step cont) k = do
  --  -- TODO: implement - put continuation into map of agents
  --  ?handle_rhs

----------------------------------------------------------------------------
-- Model implementation
-----------------------------------------------------------------------------
sirAgent' : Eff () [SIR (SIRAgent s)] [SIR (SIRAgent s')]
sirAgent' {s = Susceptible} = ?sirAgent_rhs1
sirAgent' {s = Infected}    = ?sirAgent_rhs2
sirAgent' {s = Recovered}   = ?sirAgent_rhs3

-- TODO: bring in RND effect
-- TODO: not yet acting over time
-- TODO: where is the population?
sirAgent : Eff SIRState [SIR ()] --[SIRAGENT (InitSIRAgent s)]
sirAgent = do
    init Susceptible
    -- TODO: do a exp number of times
    gotInfeced <- susceptible 5

    case gotInfeced of 
      True  => do
        rec <- recover 
        case rec of
          True  => do
            kill
            pure Recovered -- we can return any state, encode in types to return the 'correct' one
          False => do
            kill
            pure Infected -- we can return any state, encode in types to return the 'correct' one
          
      False => do
        kill
        pure Susceptible -- we can return any state, encode in types to return the 'correct' one
  where
    susceptible :  Nat
                -> Eff (Bool)
                   [SIR (SIRAgent Susceptible)] 
                   (\res => [SIR (calcInfectType res)])
    susceptible Z = pureM False
    susceptible (S k) = do
      c <- makeRandomContact
      i <- infect c
      case i of 
        True  => pureM True
        False => susceptible k

    infected : Eff (Bool)
               [SIR (SIRAgent Infected)] 
               (\res => [SIR (calcRecoverType res)])

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- running the Simulation
-----------------------------------------------------------------------------
runSIR : (sc : Nat) ->
         (i : Nat) ->
         (r : Nat) ->
         SimulationState
runSIR s i r = 
    let simState0 = mkSimulationState
        sus = createAs s Susceptible
        inf = createAs i Infected
        rec = createAs r Recovered
        (ret, simStateEnd) = runState (run sirAgent) simState0
    in  simStateEnd
  where
    -- runSIRState : Control.Monad.State.StateT SimulationState Identity SIRState
    --  runSIRState = runInit [42] sirAgent

    createAs : (n : Nat) -> (s : SIRState) -> Vect n (SIRAgent s)
    createAs Z s = []
    createAs (S k) s = MkSIRAgent Z :: createAs k s

namespace Main
  main : IO ()
  main = do
    let ss = runSIR 99 1 0
    putStrLn $ show ss
-----------------------------------------------------------------------------