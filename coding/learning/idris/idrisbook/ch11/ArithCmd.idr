module ArithCmd

import System

import InfIO
import ArithTotal

%default total

data Command : Type -> Type where
  PutStr  : String -> Command ()
  GetLine : Command String

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr str) = putStr str
runCommand GetLine      = getLine

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run (More f) (Quit v) = pure $ Just v
run (More f) (Do act cont) = do
  ret <- runCommand act
  run f (cont ret)

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do
    PutStr "Correct!\n"
    quiz nums (S score)

  wrong : Stream Int -> (score : Nat) -> (ans : Int) -> ConsoleIO Nat
  wrong nums score ans = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score = do
    PutStr ("Score so far: " ++ show score ++ "\n")
    PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
    answer <- GetLine
    if toLower answer == "quit"
      then Quit score
      else do
        if cast answer == num1 * num2
          then correct nums score 
          else wrong nums score (num1 * num2)

partial
main : IO ()
main = do 
  seed <- time
  Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
             | Nothing => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show score)