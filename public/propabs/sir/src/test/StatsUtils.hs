module StatsUtils 
  ( avgTest
  
  , tTest

  , StatsUtils.mean
  , StatsUtils.std

  , median
  
  , StatsUtils.skewness
  , StatsUtils.kurtosis
  ) where

import Data.List                        as List
import Data.Vector.Generic              as Vect

import Statistics.Distribution          as Distr
import Statistics.Distribution.StudentT as StudT
--import Statistics.Sample.Histogram      as Hist
import Statistics.Sample                as Sample

import Debug.Trace

avgTest :: [Double]
        -> Double
        -> Double
        -> Bool
avgTest ys mu0 eps 
    = abs (mu - mu0) <= eps
  where
    mu = StatsUtils.mean ys

-- Performs a t-test evaluating the null hypothesis that the mean of the population from which sample 
-- is drawn is LT/GT/EQ mu. The functions returns 'Just True' in case the null hypothesis is accepted
-- sources:
--   http://www.statisticssolutions.com/manova-analysis-one-sample-t-test/
tTest :: String
      -> [Double]
      -> Double
      -> Double
      -> Ordering
      -> Maybe Bool
tTest name samples mu0 alpha LT = tTest1Sided name samples mu0 alpha True
tTest name samples mu0 alpha GT = tTest1Sided name samples mu0 alpha False
tTest name samples mu0 alpha EQ = tTest2Sided name samples mu0 alpha

-- Performs a 2-sided t-test evaluating the null hypothesis that the mean of the population from which sample is drawn equals mu.
-- The functions returns 'Just True' in case the means are statistically equal and 'Just False' if they are not
-- In case no t-value could be calculated (in case all samples are same => no variance) Nothing is returned
-- sources:
--   http://home.apache.org/~luc/commons-math-3.6-RC2-site/jacoco/org.apache.commons.math3.stat.inference/TTest.java.html
--   https://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/stat/inference/TTest.html
tTest2Sided :: String
            -> [Double]
            -> Double
            -> Double
            -> Maybe Bool
tTest2Sided name samples mu0 alpha 
    = case mayP of
        Nothing -> Nothing -- trace (name List.++ ": cant perform t-test, t-value undefined because 0 variance!") 
        Just p  -> trace (name List.++ ": p = " List.++ show p) Just $ p > alpha
  where
    mayP = pValue $ tValue samples mu0

    pValue :: Maybe Double -> Maybe Double
    pValue Nothing  = Nothing
    pValue (Just t) = trace (name List.++ ": t = " List.++ show t) Just p
      where
        degFree = fromIntegral $ List.length samples - 1
        tDist   = StudT.studentT degFree
        tAbs    = abs t
        p       = 2 * Distr.cumulative tDist (-tAbs)

tTest1Sided :: String
            -> [Double]
            -> Double
            -> Double
            -> Bool
            -> Maybe Bool
tTest1Sided name samples mu0 alpha upper
    = case mayP of
        Nothing -> Nothing -- trace (name List.++ ": cant perform t-test, t-value undefined because 0 variance!")
        Just p  -> trace (name List.++ ": p = " List.++ show p) (if upper then Just $ p > alpha else Just $ p < alpha)
  where
    mayP = pValue $ tValue samples mu0

    pValue :: Maybe Double -> Maybe Double
    pValue Nothing  = Nothing
    pValue (Just t) = trace (name List.++ ": t = " List.++ show t) Just p
      where
        degFree = fromIntegral $ List.length samples - 1
        tDist   = StudT.studentT degFree
        p       = Distr.cumulative tDist (-t)

-- note that t-value is undefined in case of 0 variance, all samples are the same
tValue :: [Double] -> Double -> Maybe Double
tValue samples mu0
    | sigma == 0 = Nothing
    | otherwise  = trace ("n = " List.++ show n List.++ 
                          "\nmu = " List.++ show mu List.++ 
                          "\nsigma = " List.++ show sigma List.++ 
                          "\nt = " List.++ show t) Just t
  where
    n     = List.length samples
    mu    = StatsUtils.mean samples
    sigma = StatsUtils.std samples

    t = (mu - mu0) / (sigma / sqrt (fromIntegral n))

-- statistics package obviously provides mean and variance implementations
-- but they don't support simple lists ...
mean :: [Double] -> Double
mean xs = Sample.mean (Vect.fromList xs :: Sample)

std :: [Double] -> Double
std xs = Sample.stdDev (Vect.fromList xs :: Sample)

median :: [Double] -> Double
median xs 
    | odd n     = c
    | otherwise = (c + c_1) / 2
  where
    n         = List.length xs
    centerIdx = floor ((fromIntegral n / 2) :: Double)
    
    c   = xsSorted !! centerIdx
    c_1 = xsSorted !! (centerIdx - 1)

    xsSorted = List.sort xs

skewness :: [Double] -> Double
skewness xs = Sample.skewness (Vect.fromList xs :: Sample)

kurtosis :: [Double] -> Double
kurtosis xs = Sample.kurtosis (Vect.fromList xs :: Sample)

{-
histogram :: [Double] -> Int -> [(Double, Int)]
histogram xs bins = Vect.toList histVect
  where
    xsVect = Vect.fromList xs
    (intervals, samples) = Hist.histogram bins xsVect
    histVect = Vect.zip intervals samples
-}