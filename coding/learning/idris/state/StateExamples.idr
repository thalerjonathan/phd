import Control.ST
import Data.Vect

increment : (x : Var) -> STrans m () [x ::: State Integer] (const [x ::: State Integer])
increment x = do
  num <- read x
  write x (num + 1)

makeAndIncrement : Integer -> STrans m Integer [] (const [])
makeAndIncrement init = do
  var <- new init
  increment var
  x <- read var
  delete var
  pure x

ioMakeAndIncrement : STrans IO () [] (const [])
ioMakeAndIncrement = do
  lift $ putStr "Enter a number: "
  init <- lift $ getLine
  var <- new (cast init)
  lift $ putStrLn ("var = " ++ show !(read var))
  increment var
  lift $ putStrLn ("var = " ++ show !(read var))
  delete var


consoleIOMakeAndIncrement : ConsoleIO io 
                          => STrans io () [] (const [])
consoleIOMakeAndIncrement = do
  putStr "Enter a number: "
  init <- getStr
  var <- new (cast init)
  putStrLn ("var = " ++ show !(read var))
  increment var
  putStrLn ("var = " ++ show !(read var))
  delete var

readAndAdd : ConsoleIO io 
           => (vec : Var)
           -> STrans io Bool [vec ::: State (Vect n Integer)]
              (\res => [vec ::: State (if res then Vect (S n) Integer
                                              else Vect n Integer)])
readAndAdd vec = do
  putStr "Enter a number: "
  num <- getStr
  if all isDigit (unpack num)
    then returning True (update vec ((cast num) ::))
    else returning False (pure ())