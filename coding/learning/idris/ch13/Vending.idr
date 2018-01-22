VendState : Type 
VendState = (Nat, Nat)

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

data MachineCmd : Type -> VendState -> VendState -> Type where
  InsertCoin : MachineCmd () (pounds, chocs)     (S pounds, chocs)
  Vend       : MachineCmd () (S pounds, S chocs) (pounds, chocs)
  GetCoins   : MachineCmd () (pounds, chocs)     (Z, chocs)
  
  Refill     : (bars : Nat) -> MachineCmd () (Z, chocs) (Z, bars + chocs)

  Display    : String -> MachineCmd () s s
  GetInput   : MachineCmd (Maybe Input) s s

  Pure       : t -> MachineCmd t s s
  (>>=)      : MachineCmd a s1 s2 -> (a -> MachineCmd b s2 s3) -> MachineCmd b s1 s3

data MachineIO : VendState -> Type where
  Do : MachineCmd a s1 s2 -> (a -> Inf (MachineIO s2)) -> MachineIO s1 -- why s1 here and not s2 ???

namespace MachineDo
  (>>=) : MachineCmd a s1 s2 -> (a -> Inf (MachineIO s2)) -> MachineIO s1 -- why s1 here and not s2 ???
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
           InsertCoin
           machineLoop
         Just VEND => vend
         Just CHANGE => do
           GetCoins
           Display "Change returned\n"
           machineLoop
         Just (REFILL k) => refill k

parseInput : (input : String) -> Maybe Input 
parseInput input = parseInputAux (unpack (toLower input))
  where
    parseInputAux : List Char -> Maybe Input
    parseInputAux ('c' :: 'o' :: 'i' :: 'n' :: _) = Just COIN
    parseInputAux ('v' :: 'e' :: 'n' :: 'd' :: _) = Just VEND
    parseInputAux ('c' :: 'h' :: 'a' :: 'n' :: 'g' :: 'e' :: _) = Just CHANGE
    parseInputAux ('r' :: 'e' :: 'f' :: 'i' :: 'l' :: 'l' :: ' ' :: cs) 
      = case all isDigit cs of
             False => Nothing
             True  => Just $ REFILL (cast (pack cs))

runCommand : MachineCmd a inState outState -> IO a
runCommand InsertCoin = putStrLn "Coin inserted"
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

run : MachineIO s -> IO ()
run (Do cmd cont) = do
  res <- runCommand cmd
  run (cont res)

main : IO ()
main = run (machineLoop {pounds = 0} {chocs = 1})