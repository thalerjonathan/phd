import System

readNumber : IO Nat
readNumber = do
  inp <- getLine
  if all isDigit (unpack inp)
    then pure $ cast inp
    else putStrLn "Invalid input, not a number" >>= \_ => readNumber

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr "Please choose a number: "
  num <- readNumber
  case compare num target of
    LT => putStrLn (show guesses ++ ". guess is too low, try again!")
            >>= \_ => guess target (guesses + 1)
    EQ => putStrLn ("You made the correct guess after " ++ show guesses ++ " guesses")
    GT => putStrLn (show guesses ++ ". guess is too high, try again!")
            >>= \_ => guess target (guesses + 1)

main : IO ()
main = do
  t <- time 
  let r = mod t 100
  guess (cast r) 1

myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = do
  putStr prompt
  inp <- getLine
  let out = onInput inp
  putStr out
  myRepl prompt onInput

myReplWith : (state : s) -> (prompt : String) -> (onInput : s -> String -> Maybe (String, s)) -> IO ()
myReplWith state prompt onInput = do
  putStr prompt
  inp <- getLine
  let out = onInput state inp
  case out of
    Nothing => pure ()
    Just (outStr, state') => putStr outStr >>= \_ => myReplWith state' prompt onInput