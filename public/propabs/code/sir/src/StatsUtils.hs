module StatsUtils 
  ( avgTest
  , tTest

  , StatsUtils.mean
  , StatsUtils.std

  ) where

import Debug.Trace

import Statistics.Distribution          as Stat
import Statistics.Distribution.StudentT as StudT

avgTest :: [Double]
        -> Double
        -> Double
        -> Bool
avgTest ys mu0 eps 
    = abs (mu - mu0) <= eps
  where
    mu = StatsUtils.mean ys

-- a one-sided t-test with a given expected median and some confidence interval alpha
-- taken from http://www.statisticssolutions.com/manova-analysis-one-sample-t-test/
tTest :: String
      -> [Double]
      -> Double
      -> Double
      -> Maybe Bool
tTest name ys m0 alpha 
    = case mayP of
        Nothing -> trace (name ++ ": cant perform t-test, t-value undefined because 0 variance!") Nothing
        Just p  -> trace (name ++ ": p = " ++ show p) Just $ p < alpha 
  where
    mayP = pValue $ tValue ys m0

    pValue :: Maybe Double -> Maybe Double
    pValue Nothing  = Nothing
    pValue (Just t) = trace (name ++ ": t = " ++ show t) Just p
      where
        degFree = fromIntegral $ length ys - 1
        tDist   = StudT.studentT degFree
        p       = (1 - Stat.cumulative tDist (abs t)) * 2.0

-- note that t-value is undefined in case of a variance of 0: all samples are the same
tValue :: [Double]
       -> Double
       -> Maybe Double
tValue ys m0 
    | sigma == 0 = {- trace ("n = " ++ show n ++ 
                          "\nmu = " ++ show mu ++ 
                          "\nsigma = " ++ show sigma ++
                          "\nundefined t value, sigma = 0!!!") -} Nothing
    | otherwise  = {- trace ("n = " ++ show n ++ 
                          "\nmu = " ++ show mu ++ 
                          "\nsigma = " ++ show sigma ++ 
                          "\nt = " ++ show t) -} Just t
  where
    n     = length ys
    mu    = StatsUtils.mean ys
    sigma = StatsUtils.std ys

    t = (mu - m0) / (sigma / sqrt (fromIntegral n))

-- statistics package obviously provides mean and variance implementations
-- but they don't support simple lists ...
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

std :: [Double] -> Double
std xs = sqrt $ sum (map (\x -> (x - x') ** 2) xs) / (n - 1)
  where
    x' = StatsUtils.mean xs 
    n = fromIntegral (length xs)