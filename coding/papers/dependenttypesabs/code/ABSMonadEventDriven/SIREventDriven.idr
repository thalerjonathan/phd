module SIREventDriven

import Data.Vect 

import ABSMonadEventDriven

%default total

infectivity : Double
infectivity = 0.05

contactRate : Double
contactRate = 5.0

illnessDuration : Double
illnessDuration = 15.0

data SIRState 
  = Susceptible
  | Infected
  | Recovered

data SIREvent
  = TriggerContact
  | MakeContact
  | RespondContact SIRState
  | Recover

SIRAgent : (m : Type -> Type) -> Type
SIRAgent m = AgentBehaviour m SIRState SIREvent

recovered : ConsoleIO m => SIRAgent m
recovered _ = pure Recovered

infected : ConsoleIO m => SIRAgent m
infected (sender, MakeContact) = do
  schedule (RespondContact Infected) sender Z
  pure Infected
infected (_, Recover) = do
  behaviour recovered
  pure Infected
infected _ = pure Infected

susceptible : ConsoleIO m => Vect (S n) AgentId -> SIRAgent m
susceptible ais (_, TriggerContact) = do
  aid <- myId
  schedule TriggerContact aid 1

  numContacts <- randomExp contactRate
  -- TODO: make contact with 5 random agents

  pure Susceptible
susceptible _ (sender, RespondContact s) = do
  case s of
    Infected => do
      inf <- randomBool infectivity
      case inf of
        True => do
          behaviour infected
          aid <- myId 
          illDur <- randomExp illnessDuration
          schedule Recover aid (fromIntegerNat $ cast illDur)
          pure Susceptible
        False => pure Susceptible    
    _ => pure Susceptible
susceptible _ _ = pure Susceptible

sirAgent : ConsoleIO m => SIRState -> Vect (S n) AgentId -> SIRAgent m
sirAgent Susceptible ais = susceptible ais
sirAgent Infected _      = infected
sirAgent Recovered _     = recovered

-- TODO: environment is missing: solve by monad?

initSIR : ConsoleIO m =>
          (sus : Nat) ->
          (inf : Nat) ->
          (rec : Nat) ->
          (Vect n (AgentId, SIRAgent m), Vect k (Time, Event SIREvent))
initSIR sus inf rec = ?initSIR_rhs

{-
runSir : IO ()
runSir = do
  let (initAs, initEvts) = initSIR 99 1 0
  ret <- simulateUntil 150 Z initAs initEvts
  putStrLn $ show ret
-}