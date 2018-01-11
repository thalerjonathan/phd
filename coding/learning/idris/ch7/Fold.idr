totalLen : List String -> Nat
totalLen xs = foldr (\s, n => n + length s) 0 xs
