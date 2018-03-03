import System.Concurrency.Channels

%default total

data ProcState = NoRequest | Sent | Complete

data Message = Add Nat Nat 

data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

data Process : (iface : reqType -> Type) ->
               Type ->
               (in_state : ProcState) -> 
               (out_state : ProcState) ->
               Type where
     Request : MessagePID service_iface -> (msg : service_reqType) -> Process iface (service_iface msg) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) NoRequest NoRequest) -> Process iface (Maybe reqType) st Sent
     Spawn : Process service_iface () NoRequest Complete -> Process iface (Maybe (MessagePID service_iface)) st st
     Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete
     
     Action : IO a -> Process iface a st st
     Pure : a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) -> Process iface b st1 st3

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run Dry _ = pure Nothing
run f (Spawn proc) = do
  Just pid <- spawn (do run f proc
                        pure ())
       | Nothing => pure Nothing
  pure (Just (Just (MkMessage pid)))

run f (Respond {reqType} cont) = do
  Just sender <- listen 1
       | Nothing => pure (Just Nothing)
  Just msg <- unsafeRecv reqType sender
       | Nothing => pure (Just Nothing)
  Just res <- run f (cont msg)
       | Nothing => pure Nothing
  unsafeSend sender res
  pure (Just (Just msg))

run f (Request {service_iface} (MkMessage proc) msg) = do
  Just chan <- connect proc
       | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok 
    then do
      Just x <- unsafeRecv (service_iface msg) chan
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

NoRecv : Void -> Type
NoRecv = const Void

AdderType : Message -> Type
AdderType (Add x y) = Nat

Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a NoRequest Complete

Client : Type -> Type
Client a = Process NoRecv a NoRequest NoRequest

procAdder : Service AdderType ()
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
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do
  run forever proc
  pure ()

partial
main : IO ()
main = runProc procMain