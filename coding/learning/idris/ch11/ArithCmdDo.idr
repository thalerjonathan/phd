import Data.Primitives.Views
import System

data Command : Type -> Type where
  PutStr  : String -> Command ()
  GetLine : Command String

  Pure    : a -> Command a
  Bind    : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind 

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do 

data Input = Answer Int
           | QuitCmd

readInput : (promt : String) -> Command Input
readInput promt = do
  PutStr promt
  answer <- GetLine
  if toLower answer == "quit"
    then Pure QuitCmd
    else Pure (Answer (cast answer))

runCommand : Command a -> IO a
runCommand (Pure x)     = pure x
runCommand (Bind cmd f) = do
  ret <- runCommand cmd
  runCommand $ f ret
runCommand (PutStr str) = putStr str
runCommand GetLine      = getLine

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
    answer <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case answer of
      QuitCmd       => Quit score
      Answer answer => do
        if answer == num1 * num2
          then correct nums score 
          else wrong nums score (num1 * num2)

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run (More f) (Quit v) = pure $ Just v
run (More f) (Do act cont) = do
  ret <- runCommand act
  run f (cont ret)

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do 
  seed <- time
  Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
             | Nothing => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show score)