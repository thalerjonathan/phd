data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

loopPrint : String -> InfIO
loopPrint msg = Do 
  (putStrLn msg)
  (\_ => loopPrint msg) -- note: using const instead of \_ => here makes loop non total!

runInf : InfIO -> IO ()
runInf (Do action cont) = do
  ret <- action
  let cont' = cont ret
  runInf cont'

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> InfIO -> IO ()
run Dry act = putStrLn "Out of fuel"
run (More f) (Do act cont) = do
  ret <- act
  run f (cont ret)

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

loopPrintMyBind : String -> InfIO
loopPrintMyBind msg = do 
  putStrLn msg
  loopPrintMyBind msg

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = Do
  (do 
    putStr prompt
    getLine)
  (\str => Do 
    (putStr (action str))
    (\_ => totalREPL prompt action))