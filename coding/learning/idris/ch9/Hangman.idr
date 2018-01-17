import Data.Vect

import RemoveElem

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) ->
                (missing : Vect letters Char) ->
                WordState guesses_remaining letters

data Finished : Type where
  Lost : (game : WordState 0 (S letters)) -> Finished
  Won  : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidMultiple : ValidInput (c1 :: c2 :: cs) -> Void
isValidMultiple (Letter _) impossible

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)
  where
    isValidInput : (cs : List Char) -> Dec (ValidInput cs)
    isValidInput []                 = No isValidNil
    isValidInput (c :: [])          = Yes (Letter c)
    isValidInput (c1 :: (c2 :: cs)) = No isValidMultiple

processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) 
  = case isElem letter missing of
         Yes prf    => Right (MkWordState word (removeElem_auto letter missing))
         No  contra => Left (MkWordState word missing)

readGuess : IO (x ** ValidInput x)
readGuess = do
  putStrLn "Make your guess"
  str <- getLine
  case isValidString (toUpper str) of
       Yes prf    => pure (_ ** prf)
       No  contra => do
         putStrLn "Invalid input, try again"
         readGuess

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do
  (_ ** Letter letter) <- readGuess
  case processGuess letter st of
       Left l  => do
         putStrLn ("Wrong guess, there are " ++ show guesses ++ " guesses remaining.")
         case guesses of
              Z   => pure (Lost l)
              S k => game l
       Right r => do
         putStrLn ("Correct guess, there are " ++ show letters ++ " letters remaining.")
         case letters of
              Z   => pure (Won r)
              S k => game r

main : IO ()
main = do
  result <- game {guesses = 2} (MkWordState "Test" ['T', 'E', 'S'])
  case result of
       Lost game => putStrLn "You lost!"
       Won game  => putStrLn "You won!"