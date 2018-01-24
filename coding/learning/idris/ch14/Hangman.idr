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
