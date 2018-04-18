module ConcIO

import System.Concurrency.Channels

export
data Server : (iface : request -> Type) -> Type where
     MkServer : (process : PID) -> Server iface

public export
NoRecv : Void -> Type
NoRecv = const Void

public export
data ConcState = NoRequest | Sent | Complete

-- Sequences of IO commands which can spawn concurrent processes
public export
data Process : (iface : request -> Type) -> -- The interface to respond on
               Type -> -- Result of the operations
               ConcState -> ConcState -> -- The state transition
               Type where
     -- Send a message on a channel and wait for a reply (if it's a server
     -- channel)
     Send : Server server_iface -> (msg : serverreq) ->
            Process iface (server_iface msg) st st

     -- Process a client message (must terminate!)
     -- If there's no message, does nothing
     Accept : ((r : request) -> Process NoRecv (iface r) st st) ->
              Process iface () st Sent

     -- Spawn a process (which can't terminate since it's a Process)
     -- and return a channel we can send messages on
     Spawn : Process server_iface () NoRequest Complete ->
             Process iface (Maybe (Server server_iface)) st st

     -- Loop indefinitely, as long as we've sent a response
     Loop : Inf (Process iface a NoRequest Complete) ->
            Process iface a Sent Complete

     -- Some plumbing, to allow arbitrary IO actions and to bind sequences
     -- of commands together
     Action : IO a -> Process iface a st st
     Pure : a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) ->
             Process iface b st1 st3

public export
ServerLoop : (request -> Type) -> Type -> Type
ServerLoop iface t = Process iface t NoRequest Complete

public export
Client : Type -> Type
Client t = Process NoRecv t NoRequest NoRequest

public export
data Fuel = Dry | More (Lazy Fuel)

export partial
forever : Fuel
forever = More forever

export total
run : Fuel -> Process iface t st_in st_out -> IO (Maybe t)
run fuel (Send {server_iface} (MkServer process) msg)
          = do Just chan <- connect process
                    | _ => pure Nothing
               ok <- unsafeSend chan msg
               if ok then do Just x <- unsafeRecv (server_iface msg) chan
                                  | Nothing => pure Nothing
                             pure (Just x)
                     else pure Nothing
run (More fuel) (Accept {request} f)
          = do Just sender <- listen 1
                    | Nothing => pure (Just ()) -- nothing connecting
               Just msg <- unsafeRecv request sender
                    | Nothing => pure (Just ()) -- no message received
               Just res <- run fuel (f msg)
                    | Nothing => pure Nothing
               unsafeSend sender res
               pure (Just ())
run (More fuel) (Spawn x)
     = do Just pid <- spawn (do run fuel x; pure ())
               | Nothing => pure (Just Nothing)
          pure (Just (Just (MkServer pid)))
run fuel (Action x) = do x' <- x
                         pure (Just x')
run fuel (Pure x) = pure (Just x)
run (More fuel) (Loop proc) = run fuel proc
run (More fuel) (x >>= f) = do Just x' <- run fuel x
                                      | Nothing => pure Nothing
                               run fuel (f x')
run Dry _ = pure Nothing
