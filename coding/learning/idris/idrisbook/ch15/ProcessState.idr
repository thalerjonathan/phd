import System.Concurrency.Channels

%default total

data ProcState = NoRequest | Sent | Complete

data Message = Add Nat Nat 

data MessagePID = MkMessage PID

data Process : Type -> 
               (in_state : ProcState) -> 
               (out_state : ProcState) ->
               Type where
     Request : MessagePID -> Message -> Process Nat st st
     Respond : ((msg : Message) -> Process Nat NoRequest NoRequest) -> Process (Maybe Message) st Sent
     Spawn : Process () NoRequest Complete -> Process (Maybe MessagePID) st st
     Loop : Inf (Process a NoRequest Complete) -> Process a Sent Complete
     
     Action : IO a -> Process a st st
     Pure : a -> Process a st st
     (>>=) : Process a st1 st2 -> (a -> Process b st2 st3) -> Process b st1 st3

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> Process t in_state out_state -> IO (Maybe t)
run Dry _ = pure Nothing
run f (Spawn proc) = do
  Just pid <- spawn (do run f proc
                        pure ())
       | Nothing => pure Nothing
  pure (Just (Just (MkMessage pid)))

run f (Respond cont) = do
  Just sender <- listen 1
       | Nothing => pure (Just Nothing)
  Just msg <- unsafeRecv Message sender
       | Nothing => pure (Just Nothing)
  Just res <- run f (cont msg)
       | Nothing => pure Nothing
  unsafeSend sender res
  pure (Just (Just msg))

run f (Request (MkMessage proc) msg) = do
  Just chan <- connect proc
       | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok 
    then do
      Just x <- unsafeRecv Nat chan
           | Nothing => pure Nothing
      pure (Just x)
    else pure Nothing

run (More f) (Loop act) = run f act

run f (Action act)   = do
  ret <- act
  pure (Just ret)
run f (Pure val)     = pure (Just val)
run f (act >>= cont) = do
  Just ret <- run f act
       | Nothing => pure Nothing
  run f (cont ret)

Service : Type -> Type
Service a = Process a NoRequest Complete

Client : Type -> Type
Client a = Process a NoRequest NoRequest

procAdder : Service ()
procAdder = do
  Respond (\msg => case msg of 
                        Add x y => Pure (x + y))
  Loop procAdder

procMain : Client ()
procMain = do
  Just adderId <- Spawn procAdder
       | Nothing => Action (putStrLn "Spawn failed")
  answer <- Request adderId (Add 2 3)
  Action (printLn answer)

partial
forever : Fuel
forever = More forever

partial
runProc : Process () in_state out_state -> IO ()
runProc proc = do
  run forever proc
  pure ()