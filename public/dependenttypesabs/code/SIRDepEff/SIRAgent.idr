module SIRAgent

import Control.Monad.State
import Data.Vect
import Effects

import SIRState

%default total

export
AgentId : Type
AgentId = Nat

||| The data of an initialised SIR agent
-- TODO: put other data here e.g. coordinates
export
data InitSIRAgent : SIRState -> Type where
  MkInitSIRAgent : InitSIRAgent s

||| An evidence of having made contact with another agent
-- TODO: this should be only valid for the current time-step
export -- is opaque, so not possible to create
data MakeContact : Type where
  ContactWith : SIRState -> MakeContact

public export
calcInfectType : Bool -> Type
calcInfectType True  = InitSIRAgent Infected
calcInfectType False = InitSIRAgent Susceptible

public export
calcRecoverType : Bool -> Type
calcRecoverType True  = InitSIRAgent Recovered
calcRecoverType False = InitSIRAgent Infected

data SIRAgent : Effect where
  -- TODO: use sig http://docs.idris-lang.org/en/latest/effects/impleff.html
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

  --Yield : SIRAgent (InitSIRAgent s) (InitSIRAgent s) id

export
SIRAGENT : Type -> EFFECT
SIRAGENT t = MkEff t SIRAgent

export
init :  (s : SIRState) 
     -> Eff AgentId
        [SIRAGENT ()] 
        [SIRAGENT (InitSIRAgent s)]
init s = call $ Init s

export
makeRandomContact : Eff MakeContact 
                    [SIRAGENT (InitSIRAgent Susceptible)]
makeRandomContact = call MakeRandomContact

export
infect :  MakeContact
       -> Eff (Bool) 
          [SIRAGENT (InitSIRAgent Susceptible)] 
          (\res => [SIRAGENT (calcInfectType res)])
infect c = call $ Infect c

export
recover : Eff (Bool) 
          [SIRAGENT (InitSIRAgent Infected)] 
          (\res => [SIRAGENT (calcRecoverType res)])
recover = call Recover 

export
kill : Eff () 
       [SIRAGENT (InitSIRAgent s)]
       [SIRAGENT ()]
kill = call Kill

{-
-------------------------------------------------------------------------------
-- IO Handler
-------------------------------------------------------------------------------
Handler SIRAgent IO where
  handle () (Init s) k = do
    putStrLn $ "called Init " ++ show s
    k Z MkInitSIRAgent
  handle r MakeRandomContact k = do
    let con = ContactWith Infected
    putStrLn $ "called MakeRandomContact "
    k con r
  handle r (Infect (ContactWith s)) k = 
    case s of 
      Susceptible => do
        putStrLn "called Infect Contacted Susceptible => NO infection" 
        k False r
      Infected    => do
        putStrLn "called Infect Contacted Infected => infection" 
        k True MkInitSIRAgent
      Recovered   => do
        putStrLn "called Infect Contacted Recovered => NO infection" 
        k False r
  handle r Recover k = do
    putStrLn "called Recover"
    k False r
    --k True MkInitSIRAgent
  handle r Kill k = do
    putStrLn "called Kill"
    k () ()

export
runSIRInIO : IO SIRState
runSIRInIO = run sirAgent
-}

-------------------------------------------------------------------------------
-- State Handler
-------------------------------------------------------------------------------
export
record SimulationState where
  constructor MkSimulationState
  time        : Nat
  nextAgentId : AgentId

export
Show SimulationState where
  show (MkSimulationState time _) = "SimulationState t = " ++ show time

export
mkSimulationState : SimulationState
mkSimulationState = MkSimulationState Z Z

Handler SIRAgent (Control.Monad.State.StateT SimulationState Identity) where
  handle r (Init s) k = do
    ss <- get
    k Z MkInitSIRAgent
  handle r MakeRandomContact k = ?Handler_rhs_3
  handle r (Infect x) k = ?Handler_rhs_4
  handle r Recover k = ?Handler_rhs_5
  handle r Kill k = ?Handler_rhs_6

-----------------------------------------------------------------------------
-- SIR Agent model implementation
-----------------------------------------------------------------------------
sirAgent : Eff SIRState [SIRAGENT ()] --[SIRAGENT (InitSIRAgent s)]
sirAgent = do
  init Susceptible
  cont <- makeRandomContact
  inf <- infect cont

  case inf of 
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
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- running the Simulation
-----------------------------------------------------------------------------
runSIR : SimulationState
runSIR = 
  let s0       = mkSimulationState
      (ret, s) = runState (run sirAgent) s0
  in  s
  
main : IO ()
main = do
  let ss = runSIR
  putStrLn $ show ss
-----------------------------------------------------------------------------