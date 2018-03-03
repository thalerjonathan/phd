palindrome : Nat -> String -> Bool
palindrome n str = if length str > n
                    then let strLow = toLower str
                         in reverse strLow == strLow
                    else False

counts : String -> (Nat, Nat)
counts str = (length $ words str, length str)

top_ten : Ord a => List a -> List a
top_ten as = take 10 (sort as)

over_length : Nat -> List String -> Nat
over_length n ss = length $ filter (\s => length s > n) ss