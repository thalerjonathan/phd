module ABSMonadTest

import Data.Vect 

import ABSMonad

%default total

{-
data TestState
  = StateA
  | StateB

interface TestAgent (m : Type -> Type) where
  foo : Int -> Int

TestAgent IO where
  foo x = 42

data TestEvents
  = EventA
  | EventB

partial
timeInfAgent : (TestAgent m, ConsoleIO m) => 
               (t : Nat) ->
               Agent m Nat TestEvents
timeInfAgent t = do
  putStrLn $ "timeInfAgent: before noOp, t = " ++ show t
  noOp
  tSys <- time
  putStrLn $ "timeInfAgent: time from system = " ++ show tSys

  putStrLn $ "timeInfAgent: before step"
  step t timeInfAgent

-- TODO: make total, by pattern matching?
partial
timeLimitAgent : (TestAgent m, ConsoleIO m) => 
                 (tLimit : Nat) ->
                 (t : Nat) ->
                 Agent m Nat TestEvents
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
                      Agent m () TestEvents
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
                        Agent m () TestEvents
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
  as' <- simulateUntil 100 as []
  putStrLn $ show as'

partial
runSpawningAgents : IO ()
runSpawningAgents = do
  let as = [(0, spawningNumberAgent 2 Z)]
  as' <- simulateUntil 100 as []
  putStrLn $ show as'

partial
runTerminatingAgents : IO ()
runTerminatingAgents = do
  let as = [(0, terminatingAfterAgent 0 5 Z), (0, terminatingAfterAgent 1 50 Z)]
  as' <- simulateUntil 100 as []
  putStrLn $ show as'
  -}

-------------------------------------------------------------------------------
-- PING PONG
-------------------------------------------------------------------------------
data PingPongEvents
  = Ping
  | Pong

ping : ConsoleIO m => 
       AgentFunc m () PingPongEvents
ping (sender, Ping) = do
  t <- time
  aid <- myId
  putStrLn $ "ping " ++ show aid ++ " handle Ping from " ++ show sender ++ ": t = " ++ show t
  schedule Pong sender 0
ping _ = pure ()

pong : ConsoleIO m => 
       AgentFunc m () PingPongEvents
pong (sender, Pong) = do
  t <- time
  aid <- myId
  putStrLn $ "pong " ++ show aid ++ " handle Pong from " ++ show sender ++ ": t = " ++ show t
  schedule Ping sender 0
pong _ = pure ()

partial
runPingPong : IO ()
runPingPong = do
  ret <- simulateUntil 100 100 [(Z, ping), (1, pong)] [(10, (1, 0, Ping))]
  putStrLn $ show ret

-------------------------------------------------------------------------------
-- SINGLE
-------------------------------------------------------------------------------
data SingleAgentEvents
  = EventA
  | EventB

singleAgent : ConsoleIO m => 
              AgentFunc m () SingleAgentEvents
singleAgent (sender, EventA) = do
  t <- time
  aid <- myId
  putStrLn $ "singleAgent " ++ show aid ++ " handle EventA from " ++ show sender ++ ": t = " ++ show t
  schedule EventB aid 10

singleAgent (sender, EventB) = do
  t <- time
  aid <- myId
  putStrLn $ "singleAgent " ++ show aid ++ " handle EventB from " ++ show sender ++ ": t = " ++ show t
  schedule EventA aid 0

partial
runSingleAgent : IO ()
runSingleAgent = do
  ret <- simulateUntil 100 100 [(Z, singleAgent)] [(Z, (0, 0, EventA))]
  putStrLn $ show ret