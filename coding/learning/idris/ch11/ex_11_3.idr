-- 1 implemented in ArithCmdDo

%default total

data Command : Type -> Type where
  PutStr  : String -> Command ()
  GetLine : Command String

  ReadFile : (filepath : String) -> Command (Either FileError String)
  WriteFile : (filepath : String) -> (contents : String) -> Command (Either FileError ())

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

runCommand : Command a -> IO a
runCommand (ReadFile filepath) = readFile filepath
runCommand (WriteFile filepath contents) = writeFile filepath contents
runCommand (Pure x)     = pure x
runCommand (Bind cmd f) = do
  ret <- runCommand cmd
  runCommand $ f ret
runCommand (PutStr str) = putStr str
runCommand GetLine      = getLine

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry y = pure Nothing
run (More f) (Quit v) = pure $ Just v
run (More f) (Do act cont) = do
  ret <- runCommand act
  run f (cont ret)

data Input = Cat String
           | Copy String String
           | Exit

partial 
parseInput : (cmd : String) -> Maybe Input
parseInput cmd = parseCommandAux (unpack cmd)
  where
    partial -- TODO: make it total
    parseCommandAux : (cmd : List Char) -> Maybe Input
    parseCommandAux ('c' :: 'a' :: 't' :: ' ' :: cs) = Just $ Cat (pack cs)
    parseCommandAux ('c' :: 'o' :: 'p' :: 'y' :: ' ' :: cs) = do
      let ws = words (pack cs)
      if length ws /= 2
        then Nothing
        else let (w1 :: w2 :: []) = ws in
             Just $ Copy w1 w2
    parseCommandAux ('e' :: 'x' :: 'i' :: 't' :: _) = Just $ Exit
    parseCommandAux _ = Nothing

partial 
shell : ConsoleIO ()
shell = do
    input <- GetLine
    case parseInput input of
         Nothing => do
          PutStr ("Invalid command\n")
          shell
         Just (Cat f) => do
          ret <- ReadFile f
          case ret of
               Left l => do 
                 PutStr (show l ++ "\n") 
                 shell
               Right r => do
                 PutStr (r ++ "\n")
                 shell
         Just (Copy from to) => do
           ret <- ReadFile from
           case ret of
                Left l => do 
                  PutStr (show l ++ "\n") 
                  shell
                Right r => do
                  ret <- WriteFile to r
                  case ret of
                       Left l => do 
                         PutStr (show l ++ "\n") 
                         shell
                       Right r => shell
         Just Exit => Quit ()

partial
main : IO ()
main = do 
  run forever shell
  pure ()