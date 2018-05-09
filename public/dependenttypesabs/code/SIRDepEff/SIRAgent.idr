module SIRAgent

import Control.Monad.State
import Data.Vect
import Effects
import Effect.Random

import SIRState

%default total

AgentId : Type
AgentId = Nat

||| The data of an initialised SIR agent
-- TODO: put other data here e.g. coordinates
data InitSIRAgent : SIRState -> Type where
  MkInitSIRAgent : InitSIRAgent s

||| An evidence of having made contact with another agent
-- TODO: this should be only valid for the current time-step
-- is opaque, so not possible to create
data MakeContact : Type where
  ContactWith : SIRState -> MakeContact

calcInfectType : Bool -> Type
calcInfectType True  = InitSIRAgent Infected
calcInfectType False = InitSIRAgent Susceptible

calcRecoverType : Bool -> Type
calcRecoverType True  = InitSIRAgent Recovered
calcRecoverType False = InitSIRAgent Infected

data SIRAgent : Effect where
  Init : (s : SIRState) -> SIRAgent AgentId () (const $ InitSIRAgent s)
  MakeRandomContact : SIRAgent MakeContact (InitSIRAgent Susceptible) (const $ InitSIRAgent Susceptible)
  Infect : MakeContact -> SIRAgent Bool 
                          (InitSIRAgent Susceptible) 
                          (\res => calcInfectType res) -- TODO: why cant we simply use calcInfectType ?
  -- TODO: can we encode recovery over time?
  Recover : SIRAgent Bool 
            (InitSIRAgent Infected) 
            (\res => calcRecoverType res) -- TODO: why cant we simply use calcRecoverType ?
  -- TODO: remove this, SIR agents never die
  Kill : SIRAgent () (InitSIRAgent s) (\res => ())

  -- TODO: implement: passing a continuation as argument
  -- this will give the control back to the system for the next time-step
  Step : ((t : Nat) -> SIRAgent () (InitSIRAgent s) (const $ InitSIRAgent s)) 
         -> SIRAgent () (InitSIRAgent s) (const $ InitSIRAgent s)

SIRAGENT : Type -> EFFECT
SIRAGENT t = MkEff t SIRAgent

init :  (s : SIRState) 
     -> Eff AgentId
        [SIRAGENT ()] 
        [SIRAGENT (InitSIRAgent s)]
init s = call $ Init s

makeRandomContact : Eff MakeContact 
                    [SIRAGENT (InitSIRAgent Susceptible)]
makeRandomContact = call MakeRandomContact

infect :  MakeContact
       -> Eff (Bool) 
          [SIRAGENT (InitSIRAgent Susceptible)] 
          (\res => [SIRAGENT (calcInfectType res)])
infect c = call $ Infect c

recover : Eff (Bool) 
          [SIRAGENT (InitSIRAgent Infected)] 
          (\res => [SIRAGENT (calcRecoverType res)])
recover = call Recover 

kill : Eff () 
       [SIRAGENT (InitSIRAgent s)]
       [SIRAGENT ()]
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

Handler SIRAgent (Control.Monad.State.StateT SimulationState Identity) where
  handle () (Init s) k = do
    k Z MkInitSIRAgent
  handle r MakeRandomContact k = do
    let con = ContactWith Infected
    k con r
  handle r (Infect (ContactWith s)) k = 
    case s of 
      Susceptible => do
        k False r
      Infected    => do
        k True MkInitSIRAgent
      Recovered   => do
        k False r
  handle r Recover k = do
    k False r
    --k True MkInitSIRAgent
  handle r Kill k = do
    k () ()
  handle r (Step cont) k = do
    -- TODO: implement - put continuation into map of agents
    ?handle_rhs

----------------------------------------------------------------------------
-- Model implementation
-----------------------------------------------------------------------------
-- TODO: bring in RND effect
sirAgent : Eff SIRState [SIRAGENT ()] --[SIRAGENT (InitSIRAgent s)]
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
                   [SIRAGENT (InitSIRAgent Susceptible)] 
                   (\res => [SIRAGENT (calcInfectType res)])
    susceptible Z = pureM False
    susceptible (S k) = do
      c <- makeRandomContact
      i <- infect c
      case i of 
        True  => pureM True
        False => susceptible k

    infected : Eff (Bool)
               [SIRAGENT (InitSIRAgent Infected)] 
               (\res => [SIRAGENT (calcRecoverType res)])
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- running the Simulation
-----------------------------------------------------------------------------
runSIR : SimulationState
runSIR = 
  let s0       = mkSimulationState
      (ret, s) = runState (run sirAgent) s0
  in  s
  --where
  --  runSIRState : Control.Monad.State.StateT SimulationState Identity SIRState
  --  runSIRState = runInit [42] sirAgent

namespace Main
  main : IO ()
  main = do
    let ss = runSIR
    putStrLn $ show ss
-----------------------------------------------------------------------------