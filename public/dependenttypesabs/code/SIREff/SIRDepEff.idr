module SIRDepEff

import Control.Monad.State
import Data.Vect
import Effects

import Disc2dEnv
import SIRState

%default total

||| The data of an initialised SIR agent
-- TODO: put other data here e.g. coordinates
data InitSIRAgent : SIRState -> Type where
  MkInitSIRAgent : InitSIRAgent s

||| An evidence of having made contact with another agent
-- should be opaque, so only effect implementation can create it
data MakeContact : Type where
  ContactWith : SIRState -> MakeContact

calcInfectType : Bool -> Type
calcInfectType True  = InitSIRAgent Infected
calcInfectType False = InitSIRAgent Susceptible

calcRecoverType : Bool -> Type
calcRecoverType True  = InitSIRAgent Recovered
calcRecoverType False = InitSIRAgent Infected

data SIRAgent : Effect where
  -- TODO: use sig http://docs.idris-lang.org/en/latest/effects/impleff.html
  Init : (s : SIRState) -> SIRAgent () () (const $ InitSIRAgent s)
  MakeRandomContact : SIRAgent MakeContact (InitSIRAgent Susceptible) (const $ InitSIRAgent Susceptible)
  Infect : MakeContact -> sig SIRAgent Bool 
                              (InitSIRAgent Susceptible) 
                              (\res => calcInfectType res) -- TODO: why cant we simply use calcInfectType ?
  -- TODO: can we encode recovery over time?
  Recover : SIRAgent Bool 
            (InitSIRAgent Infected) 
            (\res => calcRecoverType res) -- TODO: why cant we simply use calcRecoverType ?
  -- TODO: remove this, SIR agents never die
  Kill : SIRAgent () (InitSIRAgent s) (\res => ())

  --Yield : SIRAgent (InitSIRAgent s) (InitSIRAgent s) id

SIRAGENT : Type -> EFFECT
SIRAGENT t = MkEff t SIRAgent

-- TODO: implement also a different handler e.g. State
Handler SIRAgent IO where
  handle () (Init s) k = do
    putStrLn $ "called Init " ++ show s
    k () MkInitSIRAgent
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

Handler SIRAgent (State Int) where

init :  (s : SIRState) 
     -> Eff () 
        [SIRAGENT ()] 
        [SIRAGENT (InitSIRAgent s)]
init s = call $ Init s

makeRandomContact : Eff MakeContact 
                    [SIRAGENT (InitSIRAgent Susceptible)]
makeRandomContact = call MakeRandomContact

infect : MakeContact
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

main : IO ()
main = do
  ret <- run sirAgent
  putStrLn $ "Final state = " ++ show ret