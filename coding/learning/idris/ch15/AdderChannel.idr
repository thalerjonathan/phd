import System.Concurrency.Channels

data Message = Add Nat Nat 

adder : IO ()
adder = do
  Just senderCh <- listen 1
       | Nothing => adder
  Just msg <- unsafeRecv Message senderCh
       | Nothing => adder

  case msg of
       Add x y => do
         ok <- unsafeSend senderCh (x + y)
         adder

main : IO ()
main = do
  Just adderId <- spawn adder
       | Nothing => putStrLn "Spawn failed"
  Just chan <- connect adderId
       | Nothing => putStrLn "Connection failed"
  ok <- unsafeSend chan (Add 2 3)
  Just answer <- unsafeRecv Nat chan
       | Nothing => putStrLn "Send failed"
  printLn answer

