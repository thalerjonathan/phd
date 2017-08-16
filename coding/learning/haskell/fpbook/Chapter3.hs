main :: IO ()
main = 
    do
        print $ cat [1..10] [30..40]

        print $ append [[1..10], [20..30], [30..40]]

        print $ min' []

        print $ min' [1, -1]

cat :: [a] -> [a] -> [a]
cat [] bs = bs
cat (a:as) bs = a : cat as bs
--cat as bs = foldr (:) as bs

append :: [[a]] -> [a]
append [] = []
append (as:ass) = cat as (append ass)

min' :: [Double] -> Double
min' [] = 1/0
min' (a:as) = if a < ma then a else ma
    where
        ma = min' as

min'' :: (Ord a) => [a] -> a
min'' [] = error "no min found in empty list"
min'' (a:[]) = a
min'' (a:as) = if a < ma then a else ma
    where
        ma = min'' as