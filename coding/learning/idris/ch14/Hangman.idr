import Data.Vect

%default total

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GuessResult = Correct | Incorrect

data GameState : Type where
     NotRunning : GameState
     Running : (guesses : Nat) -> (letters : Nat) -> GameState

data GameCmd : (t : Type) -> GameState -> (t -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (letters word))))
     Won : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S guesses)) (const NotRunning)

     Guess : (c : Char) ->
             GameCmd GuessResult 
              (Running (S guesses) (S letters))
              (\res => case res of 
                            Correct => Running (S guesses) letters
                            Incorrect => Running guesses (S letters))

     ShowState : GameCmd () state (const state)
     Message : String -> GameCmd () state (const state)
     ReadGuess : GameCmd Char state (const state)

     Pure : (res : t) -> GameCmd t (state_fn res) state_fn
     (>>=) : GameCmd a s1 s2_fn -> 
             ((res : a) -> GameCmd b (s2_fn res) s3_fn) -> 
             GameCmd b s1 s3_fn

namespace Loop
  data GameLoop : (t : Type) -> GameState -> (t -> GameState) -> Type where
       (>>=) : GameCmd a s1 s2_fn -> 
               ((res : a) -> Inf (GameLoop b (s2_fn res) s3_fn)) -> 
               GameLoop b s1 s3_fn
       Exit : GameLoop () NotRunning (const NotRunning)

gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
gameLoop {guesses} {letters} = do
  ShowState
  g <- ReadGuess
  ok <- Guess g
  case ok of
       Correct   => case letters of
                         Z => do
                           Won
                           ShowState
                           Exit
                         (S k) => do
                           Message "Correct"
                           gameLoop
       Incorrect => case guesses of
                         Z     => do 
                           Lost
                           ShowState
                           Exit
                         (S k) => do
                           Message "Incorrect"
                           gameLoop

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do
  NewGame "testing"
  gameLoop

data Game : GameState -> Type where
     GameStart : Game NotRunning
     GameWon   : (word : String) -> Game NotRunning
     GameLost  : (word : String) -> Game NotRunning
     InProgress : (word : String) -> (guesses : Nat) ->
                  (missing : Vect letters Char) ->  
                  Game (Running guesses letters)

Show (Game g) where
  show GameStart = "Starting"
  show (GameWon word) = "Game won, word was: " ++ word
  show (GameLost word) = "Game lost, word was: " ++ word
  show (InProgress word guesses missing) 
      = "\n" ++ pack (map hideMissing (unpack word))
            ++ "\n" ++ show guesses ++ " guesses left"
    where
      hideMissing : Char -> Char
      hideMissing c = if c `elem` missing then '-' else c

data Fuel = Dry | More (Lazy Fuel)

data GameResult : (t : Type) -> (t -> GameState) -> Type where
     OK : (res : t) -> Game (outState_fn res) -> GameResult t outState_fn
     OutOfFuel : GameResult t outState_fn

ok : (res : t) -> Game (outState_fn res) -> IO (GameResult t outState_fn)
ok res st = pure (OK res st)

-- copy from chapter 9: RemoveElem.idr
removeElem : (value : a) -> 
             (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} -> 
             Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = There later} = y :: removeElem value ys

runCmd : Fuel -> Game instate -> GameCmd t instate outState_fn -> IO (GameResult t outState_fn)
runCmd fuel state (NewGame word) = ok () (InProgress (toUpper word) _ (fromList (letters word)))
runCmd fuel (InProgress word _ _) Won = ok () (GameWon word)
runCmd fuel (InProgress word _ _) Lost = ok () (GameLost word)
runCmd fuel (InProgress word _ missing) (Guess c) 
  = case isElem c missing of
         Yes prf   => ok Correct (InProgress word _ (removeElem c missing))
         No contra => ok Incorrect (InProgress word _ missing)
runCmd fuel state ShowState = do
  printLn state
  ok () state
runCmd fuel state (Message str) = do
  putStrLn str
  ok () state
runCmd (More fuel) state ReadGuess = do
  putStr "Guess: "
  input <- getLine
  case unpack input of
       [x] => if isAlpha x
                then ok (toUpper x) state
                else do
                  putStrLn "Invalid input"
                  runCmd fuel state ReadGuess
       _   => do
         putStrLn "Invalid input"
         runCmd fuel state ReadGuess
runCmd Dry _ _ = pure OutOfFuel
runCmd fuel state (Pure res) = ok res state
runCmd fuel state (cmd >>= cont) = do
  OK cmdRes st' <- runCmd fuel state cmd 
  | OutOfFuel => pure OutOfFuel
  runCmd fuel st' (cont cmdRes)
  
run : Fuel -> 
      Game instate -> 
      GameLoop t instate outState_fn -> 
      IO (GameResult t outState_fn)
run Dry _ _ = pure OutOfFuel
run (More f) st (cmd >>= cont) = do
  res <- runCmd f st cmd 
  case res of
       OK cmdRes st' => run f st' (cont cmdRes)
       OutOfFuel => pure OutOfFuel
run (More f) st Exit = ok () st

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do
  run forever GameStart hangman
  pure ()