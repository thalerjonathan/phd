import System.Concurrency.Channels

%default total

data Message = Add Nat Nat 

data MessagePID = MkMessage PID

data Process : Type -> Type where
     Spawn : Process () -> Process (Maybe MessagePID)
     Request : MessagePID -> Message -> Process (Maybe Nat)
     Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
     Loop : Inf (Process a) -> Process a

     Action : IO a -> Process a
     Pure   : a -> Process a
     (>>=)  : Process a -> (a -> Process b) -> Process b

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> Process t -> IO (Maybe t)
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
       | _ => pure (Just Nothing)
  ok <- unsafeSend chan msg
  if ok 
    then do
      Just x <- unsafeRecv Nat chan
           | Nothing => pure (Just Nothing)
      pure (Just (Just x))
    else pure (Just Nothing)

run (More f) (Loop act) = run f act

run f (Action act)   = do
  ret <- act
  pure (Just ret)
run f (Pure val)     = pure (Just val)
run f (act >>= cont) = do
  Just ret <- run f act
       | Nothing => pure Nothing
  run f (cont ret)

procAdder : Process ()
procAdder = do
  Respond (\msg => case msg of 
                        Add x y => Pure (x + y))
  Loop procAdder

procMain : Process ()
procMain = do
  Just adderId <- Spawn procAdder
       | Nothing => Action (putStrLn "Spawn failed")
  Just answer <- Request adderId (Add 2 3)
       | Nothing => Action (putStrLn "Request failed")
  Action (printLn answer)

partial
forever : Fuel
forever = More forever

partial
runProc : Process () -> IO ()
runProc proc = do
  run forever proc
  pure ()