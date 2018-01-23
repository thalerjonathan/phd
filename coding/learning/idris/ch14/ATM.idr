import Data.Vect

PIN : Type
PIN = Vect 4 Char

data ATMState = Ready | CardInserted | Session

data PINCheck = CorrectPIN | IncorrectPIN

data HasCard : ATMState -> Type where
     HasCI      : HasCard CardInserted
     HasSession : HasCard Session

data ATMCmd : (t : Type) -> ATMState -> (t -> ATMState) -> Type where
     InsertCard : ATMCmd ()  Ready        (const CardInserted)
     EjectCard  : {auto prf : HasCard state} -> ATMCmd () state (const Ready)
     GetPIN     : ATMCmd PIN CardInserted (const CardInserted)

     CheckPIN   : PIN -> ATMCmd PINCheck CardInserted
                          (\check => case check of 
                                          CorrectPIN => Session
                                          IncorrectPIN => CardInserted)
     GetAmount : ATMCmd Nat state (const state)

     Dispense  : (amount : Nat) -> ATMCmd () Session (const Session)

     Message : String -> ATMCmd () state (const state)
     Pure    : (res : t) -> ATMCmd t (state_fn res) state_fn
     (>>=)   : ATMCmd a state1 state2_fn -> 
               ((res : a) -> ATMCmd b (state2_fn res) state3_fn) ->
               ATMCmd b state1 state3_fn

{-
badATM : ATMCmd () Ready (const Ready)
badATM = EjectCard 
-}

atm : ATMCmd () Ready (const Ready)
atm = do
  InsertCard
  pin <- GetPIN
  pinOK <- CheckPIN pin
  Message "Checking Card"
  case pinOK of
       CorrectPIN   => do
         cash <- GetAmount
         Dispense cash
         EjectCard
         Message "Please remove your card and cash"
       IncorrectPIN => do
         Message "Incorrect PIN"
         EjectCard

testPIN : Vect 4 Char
testPIN = ['1', '2', '3', '4']

readVect : (n : Nat) -> IO (Vect n Char)
readVect Z = do
  discard <- getLine
  pure []
readVect (S k) = do
  ch <- getChar
  chs <- readVect k
  pure (ch :: chs)

runATM : ATMCmd res inState outState_fn -> IO res
runATM InsertCard = do
  putStrLn "Please insert your card (press enter)"
  x <- getLine 
  pure ()
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = do
  putStr "Enter Pin: "
  readVect 4
runATM (CheckPIN pin) = 
  case pin == testPIN of
       False => pure IncorrectPIN
       True  => pure CorrectPIN
runATM GetAmount = do
  putStrLn "How much would you like?"
  x <- getLine
  pure (cast x)
runATM (Dispense amount) = putStrLn ("Here is " ++ show amount)
runATM (Message msg) = putStr msg
runATM (Pure res) = pure res
runATM (cmd >>= cont) = do
  ret <- runATM cmd 
  runATM (cont ret)