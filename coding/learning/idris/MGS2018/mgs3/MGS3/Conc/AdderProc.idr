import Process

data Request = Add Nat Nat

Response : Request -> Type
Response (Add x y) = Nat

total
adder : ServerLoop Response ()
adder = do Action (putStrLn "Waiting")
           Accept (\msg => case msg of
                              Add x y => Pure (x + y))
           Loop adder

client : Server Response -> Client ()
client addServer = do Action (putStr "Number: ")
                      num <- Action getLine
                      answer <- Send addServer (Add (cast num) 94)
                      Action (printLn answer)

conc_main : Client ()
conc_main = do Just addServer <- Spawn adder
                    | Nothing => Action (putStrLn "Spawn failed")
               client addServer

partial main : IO ()
main = do run forever conc_main
          pure ()
