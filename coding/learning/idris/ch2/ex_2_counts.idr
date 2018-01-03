module Main

-- counts needs to be defined before referenced by main...
counts : String -> (Nat, Nat)
counts str = (length $ words str, length str)

main : IO ()
main = repl 
        "Enter a string: " 
        ((\str => str ++ "\n") . show . counts)