import Data.Primitives.Views
import System

%default total

VendState : Type 
VendState = (Nat, Nat)

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

data CoinResult = Inserted
                | Rejected

data MachineCmd : (t : Type) -> VendState -> (t -> VendState) -> Type where
  InsertCoin : MachineCmd CoinResult (pounds, chocs) (\res => case res of 
                                                                   Inserted => (S pounds, chocs)
                                                                   Rejected => (pounds, chocs))
  Vend       : MachineCmd () (S pounds, S chocs) (const (pounds, chocs))
  GetCoins   : MachineCmd () (pounds, chocs)     (const (Z, chocs))
  
  Refill     : (bars : Nat) -> MachineCmd () (Z, chocs) (const (Z, bars + chocs))

  Display    : String -> MachineCmd () s (const s)
  GetInput   : MachineCmd (Maybe Input) s (const s)

  Pure       : (res : t) -> MachineCmd t (state_fn res) state_fn
  (>>=)      : MachineCmd a s1 s2_fn ->
               ((res : a) -> MachineCmd b (s2_fn res) s3_fn) ->
               MachineCmd b s1 s3_fn

data MachineIO : VendState -> Type where
  Do : MachineCmd a s1 s2_fn -> ((res : a) -> Inf (MachineIO (s2_fn res))) -> MachineIO s1 -- why s1 here and not s2 ???

namespace MachineDo
  (>>=) : MachineCmd a s1 s2_fn -> ((res : a) -> Inf (MachineIO (s2_fn res))) -> MachineIO s1 -- why s1 here and not s2 ???
  (>>=) = Do

mutual
  vend : MachineIO (pounds, chocs)
  vend {pounds = S p} {chocs = S c} = do
    Vend
    Display "Enjoy!"
    machineLoop
  vend {pounds = Z} = do
    Display "Please insert coin!\n"
    machineLoop
  vend {chocs = Z} = do
    Display "Machine empty!\n"
    machineLoop

  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num = do
    Refill num
    machineLoop
  refill _ = do
    Display "Can't refill, coins in machine!\n"
    machineLoop

  machineLoop : MachineIO (pounds, chocs)
  machineLoop = do
    input <- GetInput
    case input of
         Nothing => do
           Display "Invalid Input\n"
           machineLoop
         Just COIN => do
           ret <- InsertCoin
           case ret of
                Inserted => do
                  Display "Coin inserted\n"
                  machineLoop
                Rejected => do
                  Display "Coin rejected\n"
                  machineLoop
         Just VEND => vend
         Just CHANGE => do
           GetCoins
           Display "Change returned\n"
           machineLoop
         (Just (REFILL k)) => refill k

parseInput : (input : String) -> Maybe Input 
parseInput input = parseInputAux (unpack (toLower input))
  where
    parseInputAux : List Char -> Maybe Input
    parseInputAux ('c' :: 'o' :: 'i' :: 'n' :: []) = Just COIN
    parseInputAux ('v' :: 'e' :: 'n' :: 'd' :: []) = Just VEND
    parseInputAux ('c' :: 'h' :: 'a' :: 'n' :: 'g' :: 'e' :: []) = Just CHANGE
    parseInputAux ('r' :: 'e' :: 'f' :: 'i' :: 'l' :: 'l' :: ' ' :: cs) 
      = case all isDigit cs of
             False => Nothing
             True  => Just $ REFILL (cast (pack cs))
    parseInputAux _ = Nothing

randomCoinResult : IO CoinResult
randomCoinResult = do
    seed <- time 
    pure $ randomCoinResultAux seed
  where
    randomCoinResultAux : Integer -> CoinResult
    randomCoinResultAux seed with (divides seed 10)
      randomCoinResultAux ((10 * div) + rem) | (DivBy prf) 
        = case rem > 3 of
               False => Rejected
               True => Inserted

runCommand : MachineCmd a inState outState_fn -> IO a
runCommand InsertCoin = randomCoinResult 
runCommand Vend = putStrLn "Chocolate vended"
runCommand {inState = (pounds, _)} GetCoins 
  = putStrLn ("Returning " ++ show pounds ++ " pounds")
runCommand (Refill bars) 
  = putStrLn ("Refilled to " ++ show bars ++ " bars")
runCommand (Display str) = putStr str
runCommand {inState = (pounds, chocs)} GetInput = do
  putStrLn ("Coins: " ++ show pounds ++ "; " ++ "Stock: " ++ show chocs)
  str <- getLine
  pure $ parseInput str 
runCommand (Pure x) = pure x
runCommand (cmd >>= cont) = do
  ret <- runCommand cmd
  runCommand (cont ret)

partial
run : MachineIO s -> IO ()
run (Do cmd cont) = do
  res <- runCommand cmd
  run (cont res)

partial
main : IO ()
main = run (machineLoop {pounds = 0} {chocs = 1})