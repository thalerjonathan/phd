import InfIO

%default total

data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do   : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do
  putStrLn "Enter your name: "
  name <- getLine
  if name == ""
    then do
      putStrLn "Bye bye!"
      Quit ()
    else do
      putStrLn $ "Hello " ++ name
      greet

run : Fuel -> RunIO a -> IO (Maybe a)
run f (Quit x) = pure $ Just x
run Dry p = pure Nothing
run (More f) (Do act cont) = do
  ret <- act 
  run f (cont ret)

partial
main : IO ()
main = do 
  run forever greet
  pure ()