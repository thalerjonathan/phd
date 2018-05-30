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
-}

-------------------------------------------------------------------------------
-- SPAWNING AGENTS
-------------------------------------------------------------------------------
spawning : ConsoleIO m => Nat -> AgentFunc m () ()
spawning Z (_, ()) = do
  t <- time
  aid <- myId
  putStrLn $ "spawning " ++ show aid ++ ": finished spawning will terminate, t = " ++ show t
  terminate
spawning (S n) (_, ()) = do
  t <- time
  aid <- myId
  putStrLn $ "spawning " ++ show aid ++ ": before spawn, t = " ++ show t
  newAid <- spawn (spawning n)
  schedule () newAid 1
  putStrLn $ "spawning " ++ show aid ++ ": after spawn newAid = " ++ show newAid ++ ", " ++ show n ++ " spawns left"
  behaviour (spawning n)
  schedule () aid 1

partial
runSpawningAgents : IO ()
runSpawningAgents = do
  ret <- simulateUntil 100 100 [(Z, spawning 2)] [(10, (0, 0, ()))]
  putStrLn $ show ret
  
-------------------------------------------------------------------------------
-- CHANGING BEHAVIOUR
-------------------------------------------------------------------------------
mutual
  partial
  behaviourA : ConsoleIO m => AgentFunc m () ()
  behaviourA (_, ()) = do
    t <- time
    aid <- myId
    putStrLn $ "behaviourA: t = " ++ show t
    behaviour behaviourB
    schedule () aid 1

  partial
  behaviourB : ConsoleIO m => AgentFunc m () ()
  behaviourB (_, ()) = do
    t <- time
    aid <- myId
    putStrLn $ "behaviourB: t = " ++ show t
    behaviour behaviourA
    schedule () aid 10

partial
runChangingBehaviour : IO ()
runChangingBehaviour = do
  ret <- simulateUntil 100 100 [(Z, behaviourA)] [(10, (0, 0, ()))]
  putStrLn $ show ret

-------------------------------------------------------------------------------
-- TERMINATING
-------------------------------------------------------------------------------
foreverAgent : ConsoleIO m => 
               AgentFunc m () ()
foreverAgent (_, ()) = do
  t <- time
  aid <- myId
  putStrLn $ "foreverAgent: t = " ++ show t
  schedule () aid 1

terminatingAgent : ConsoleIO m => 
                   AgentFunc m () ()
terminatingAgent (_, ()) = do
  t <- time
  aid <- myId
  putStrLn $ "terminatingAgent: t = " ++ show t

  case t > 42 of
    False => schedule () aid 10
    True  => do
      putStrLn $ "terminatingAgent terminate"
      terminate

partial
runTerminatingAgent : IO ()
runTerminatingAgent = do
  --ret <- simulateUntil 100 100 [(Z, terminatingAgent), (1, foreverAgent)] [(Z, (0, 0, ())), (Z, (1, 1, ()))]
  ret <- simulateUntil 100 100 [(Z, terminatingAgent)] [(Z, (0, 0, ()))]
  putStrLn $ show ret

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
  schedule Pong sender 0 -- NOTE: can have a time-delay or none
ping _ = pure ()

pong : ConsoleIO m => 
       AgentFunc m () PingPongEvents
pong (sender, Pong) = do
  t <- time
  aid <- myId
  putStrLn $ "pong " ++ show aid ++ " handle Pong from " ++ show sender ++ ": t = " ++ show t
  schedule Ping sender 0 -- NOTE: can have a time-delay or none
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