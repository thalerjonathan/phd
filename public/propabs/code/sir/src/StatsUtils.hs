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

-- Performs a 2-sided t-test evaluating the null hypothesis that the mean of the population from which sample is drawn equals mu.
-- The functions returns 'Just True' in case the means are statistically equal and 'Just False' if they are not
-- In case no t-value could be calculated (in case all samples are same => no variance) Nothing is returned
--
-- To test the 2-sided hypothesis sample mean = mu at the 95% level, use 'tTest "testId" samples mu 0.05'
--
-- sources:
--   http://www.statisticssolutions.com/manova-analysis-one-sample-t-test/
--   http://home.apache.org/~luc/commons-math-3.6-RC2-site/jacoco/org.apache.commons.math3.stat.inference/TTest.java.html
--   https://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/stat/inference/TTest.html
tTest :: String
      -> [Double]
      -> Double
      -> Double
      -> Maybe Bool
tTest name samples mu0 alpha 
    = case mayP of
        Nothing -> trace (name ++ ": cant perform t-test, t-value undefined because 0 variance!") Nothing
        Just p  -> trace (name ++ ": p = " ++ show p) Just $ p < alpha
  where
    mayP = pValue tValue

    pValue :: Maybe Double -> Maybe Double
    pValue Nothing  = Nothing
    pValue (Just t) = trace (name ++ ": t = " ++ show t) Just p
      where
        degFree = fromIntegral $ length samples - 1
        tDist   = StudT.studentT degFree
        tAbs    = abs t
        p       = 2 * Stat.cumulative tDist (-tAbs)

    -- note that t-value is undefined in case of 0 variance, all samples are the same
    tValue :: Maybe Double
    tValue
        | sigma == 0 = {- trace ("n = " ++ show n ++ 
                              "\nmu = " ++ show mu ++ 
                              "\nsigma = " ++ show sigma ++
                              "\nundefined t value, sigma = 0!!!") -} Nothing
        | otherwise  = {- trace ("n = " ++ show n ++ 
                              "\nmu = " ++ show mu ++ 
                              "\nsigma = " ++ show sigma ++ 
                              "\nt = " ++ show t) -} Just t
      where
        n     = length samples
        mu    = StatsUtils.mean samples
        sigma = StatsUtils.std samples

        t = (mu - mu0) / (sigma / sqrt (fromIntegral n))

-- statistics package obviously provides mean and variance implementations
-- but they don't support simple lists ...
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

std :: [Double] -> Double
std xs = sqrt $ sum (map (\x -> (x - x') ** 2) xs) / (n - 1)
  where
    x' = StatsUtils.mean xs 
    n = fromIntegral (length xs)