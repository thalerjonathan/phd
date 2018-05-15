module ABSMonadTest

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
               Agent m Nat t
timeInfAgent t = step (\t' => do
  putStrLn $ "t = " ++ show t ++ ", t' = " ++ show t'
  timeInfAgent t')

-- TODO: make total, by pattern matching
partial
time42Agent : (TestAgent m, ConsoleIO m) => 
              (t : Nat) ->
              Agent m Nat t
time42Agent t = step (\t' => do
  putStrLn $ "t = " ++ show t ++ ", t' = " ++ show t'
  case compare t' 42 of
    LT => time42Agent t'
    _  => pure t')

reactAfterAgent : (TestAgent m, ConsoleIO m) => 
                  (t : Nat) ->
                  Agent m Nat t
reactAfterAgent t = ?reactAfterAgent_rhs

partial
runSingleAgent : IO ()
runSingleAgent = do
  ret <- runAgent 0 (timeInfAgent Z) Z
  putStrLn $ show ret

partial
runTwoAgents : IO ()
runTwoAgents = do
  ret <- runAgents 0 (timeInfAgent Z) Z
  putStrLn $ show ret