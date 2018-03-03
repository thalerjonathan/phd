import InfIO

greet : InfIO
greet = do
  putStr "Enter your name: "
  name <- getLine
  putStrLn ("Hello " ++ name)
  greet