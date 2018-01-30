import System.Concurrency.Channels

data Message = Add Nat Nat 

data MessagePID = MkMessage PID

data Process : Type -> Type where
     Action : IO a -> Process a
     Spawn : Process () -> Process (Maybe MessagePID)
     Request : MessagePID -> Message -> Process (Maybe Nat)
     Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)

     Pure   : a -> Process a
     (>>=)  : Process a -> (a -> Process b) -> Process b

run : Process t -> IO t
run (Action act)   = act

run (Spawn proc) = do
  Just pid <- spawn (run proc)
       | Nothing => pure Nothing
  pure (Just (MkMessage pid))

run (Respond cont) = do
  Just sender <- listen 1
       | Nothing => pure Nothing
  Just msg <- unsafeRecv Message sender
       | Nothing => pure Nothing
  res <- run (cont msg)
  unsafeSend sender res
  pure (Just msg)

run (Request (MkMessage proc) msg) = do
  Just chan <- connect proc
       | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok 
    then do
      Just x <- unsafeRecv Nat chan
           | Nothing => pure Nothing
      pure (Just x)
    else pure Nothing

run (Pure val)     = pure val
run (act >>= cont) = do
  ret <- run act
  run (cont ret)

procAdder : Process ()
procAdder = do
  Respond (\msg => case msg of 
                        Add x y => Pure (x + y))
  procAdder

procMain : Process ()
procMain = do
  Just adderId <- Spawn procAdder
       | Nothing => Action (putStrLn "Spawn failed")
  Just answer <- Request adderId (Add 2 3)
       | Nothing => Action (putStrLn "Request failed")
  Action (printLn answer)