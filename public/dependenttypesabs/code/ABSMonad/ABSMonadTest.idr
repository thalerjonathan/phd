module ABSMonadTest

import Data.Vect 

import ABSMonad

%default total

data TestState
  = StateA
  | StateB

interface TestAgent (m : Type -> Type) where
  foo : Int -> Int
TestAgent IO where
  foo x = 42

partial
timeInfAgent : (TestAgent m, ConsoleIO m) => 
               (t : Nat) ->
               Agent m Nat
timeInfAgent t = do
  putStrLn $ "timeInfAgent: before noOp, t = " ++ show t
  noOp
  putStrLn $ "timeInfAgent: before noOp"
  noOp
  putStrLn $ "timeInfAgent: before step"

  step t timeInfAgent
  step t timeInfAgent

-- TODO: make total, by pattern matching?
partial
timeLimitAgent : (TestAgent m, ConsoleIO m) => 
                 (tLimit : Nat) ->
                 (t : Nat) ->
                 Agent m Nat
timeLimitAgent tLimit t = do
  putStrLn $ "timeLimitAgent: before noOp, t = " ++ show t
  noOp
  putStrLn $ "timeLimitAgent: before noOp"
  noOp
  putStrLn $ "timeLimitAgent: before step"

  case compare t tLimit of
      LT => step t (timeLimitAgent tLimit)
      _  => pure t

spawningNumberAgent : (TestAgent m, ConsoleIO m) => 
                      (n : Nat) ->
                      (t : Nat) ->
                      Agent m ()
spawningNumberAgent Z t = do
  putStrLn $ "spawningNumberAgent: finished spawning, t = " ++ show t
  pure ()
spawningNumberAgent (S n) t = do
  putStrLn $ "spawningNumberAgent: before spawn, t = " ++ show t
  spawn (spawningNumberAgent n t)
  putStrLn $ "spawningNumberAgent: after spawn, " ++ show n ++ " spawns left"
  step () (spawningNumberAgent n)

partial
terminatingAfterAgent : (TestAgent m, ConsoleIO m) => 
                        AgentId ->
                        (tLimit : Nat) ->
                        (t : Nat) ->
                        Agent m ()
terminatingAfterAgent aid tLimit t = do
  putStrLn $ "terminatingAfterAgent " ++ show aid ++ ": before check, t = " ++ show t
  case compare t tLimit of
      LT => do
        putStrLn $ "terminatingAfterAgent " ++ show aid ++ ": not yet time to terminate..."
        step () (terminatingAfterAgent aid tLimit)
      _  => do
        putStrLn $ "terminatingAfterAgent " ++ show aid ++ ": its time to terminate!"
        terminate

partial
runTimeAgents : IO ()
runTimeAgents = do
  let as = [(1, timeInfAgent Z)]
  as' <- runAgentsUntil 100 as
  putStrLn $ show as'

partial
runSpawningAgents : IO ()
runSpawningAgents = do
  let as = [(0, spawningNumberAgent 2 Z)]
  as' <- runAgentsUntil 100 as
  putStrLn $ show as'

partial
runTerminatingAgents : IO ()
runTerminatingAgents = do
  let as = [(0, terminatingAfterAgent 0 5 Z), (0, terminatingAfterAgent 1 50 Z)]
  as' <- runAgentsUntil 100 as
  putStrLn $ show as'