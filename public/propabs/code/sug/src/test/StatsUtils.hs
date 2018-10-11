module StatsUtils 
  ( std
  , mean
  ) where

-- statistics package obviously provides mean and variance implementations
-- but they don't support simple lists ...
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

std :: [Double] -> Double
std xs = sqrt $ sum (map (\x -> (x - x') ** 2) xs) / (n - 1)
  where
    x' = mean xs 
    n = fromIntegral (length xs)