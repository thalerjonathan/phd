import Data.Primitives.Views
import System

record Score where
  constructor MkScore
  correct : Nat
  attempted : Nat

record GameState where
  constructor MkGameState
  score : Score
  difficulty : Int

initState : GameState
initState = MkGameState (MkScore 0 0) 12


Show GameState where
  show st = show (correct $ score st) ++ "/" ++ 
            show (attempted $ score st) ++ "\nDifficulty: " ++ 
            show (difficulty st)

setDifficulty : Int -> GameState -> GameState
setDifficulty d s = record { difficulty = d } s

addWrong : GameState -> GameState
addWrong s = record { score -> attempted $= (+1) } s

addCorrect : GameState -> GameState
addCorrect s = record { score -> correct $= (+1),
                        score -> attempted $= (+1) } s

data Command : Type -> Type where
  PutStr  : String -> Command ()
  GetLine : Command String

  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()

  Pure    : a -> Command a
  Bind    : Command a -> (a -> Command b) -> Command b

mutual
  Functor Command where
    map f cmd = do
      cmd' <- cmd
      pure (f cmd')

  Applicative Command where
    pure = Pure
    (<*>) f cmd = do
      f' <- f
      cmd' <- cmd
      pure (f' cmd')

  Monad Command where
    (>>=) = Bind
 
data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind 

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do 

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

runCommand : Stream Int -> GameState -> Command a -> IO (a, Stream Int, GameState)
runCommand rands s (PutStr str) = do
  putStr str 
  pure ((), rands, s)
runCommand rands s GetLine = do
  str <- getLine
  pure (str, rands, s)
runCommand (r :: rands) s GetRandom 
    = let r = getRandom r (difficulty s) in
          pure (r, rands, s)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1
runCommand rands s GetGameState = pure (s, rands, s)
runCommand rands s (PutGameState s') = pure ((), rands, s')
runCommand rands s (Pure a) = pure (a, rands, s)
runCommand rands s (Bind cmd cont) = do
  (a, rands', s') <- runCommand rands s cmd
  runCommand rands' s' (cont a)

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do
  s <- GetGameState
  PutGameState (f s)

data Input = Answer Int
           | QuitCmd

mutual
  correct : ConsoleIO GameState
  correct = do
    PutStr "Correct!\n"
    updateGameState addCorrect
    quiz
  
  wrong : Int -> ConsoleIO GameState
  wrong ans = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    updateGameState addWrong
    quiz

  readInput : (promt : String) -> Command Input
  readInput promt = do
    PutStr promt
    answer <- GetLine
    if toLower answer == "quit"
      then Pure QuitCmd
      else Pure (Answer (cast answer))

  quiz : ConsoleIO GameState
  quiz = do
    num1 <- GetRandom
    num2 <- GetRandom
    st   <- GetGameState
    PutStr (show st ++ "\n")

    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
        Answer answer => 
          if answer == num1 * num2 
            then correct
            else wrong (num1 * num2)
        QuitCmd => Quit st

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run Dry rands s act = pure (Nothing, rands, s)
run (More f) rands s (Quit a) = pure (Just a, rands, s)
run (More f) rands s (Do cmd cont) = do
  (a, rands', s') <- runCommand rands s cmd 
  run f rands' s' (cont a)

partial
main : IO ()
main = do
  seed <- time
  (Just score, _, state) <-
    run forever (randoms (fromInteger seed)) initState quiz
        | _ => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show state)