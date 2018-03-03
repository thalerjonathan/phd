module Main

-- palindrome needs to be defined before referenced by main...
palindrome : String -> Bool
palindrome s = reverse s == s

main : IO ()
main = repl
        "Enter a string: "
        ((\str => str ++ "\n") . show . palindrome)
