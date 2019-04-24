module Utils.Stats 
  ( TTestTail (..)
  , tTestSamples

  , pValueTwoTailed
  , pValueUpperTailed
  , pValueLowerTailed
  , tValue

  , avgTest

  , Utils.Stats.mean
  , Utils.Stats.std

  , expCDF
  
  , median

  , nearlyEqual
  
  , Utils.Stats.skewness
  , Utils.Stats.kurtosis
  ) where

import Data.Maybe
import Data.List as List
import Data.Vector.Generic as Vect

import Statistics.Distribution  as Distr
import Statistics.Distribution.StudentT as StudT
import Statistics.Distribution.Exponential as Exp
import Statistics.Sample as Sample

--------------------------------------------------------------------------------
-- NOTE: THIS MODULE WAS VALIDATED AND SHOULD BE CORRECT!
-- Sources
--   http://www.statisticssolutions.com/manova-analysis-one-sample-t-test/
--   https://en.wikibooks.org/wiki/Statistics/Testing_Data/t-tests 
--   
-- let mu0   = 60
--     mu    = 50.2
--     s     = 2.5
--     n     = 20
--     alpha = 0.05 :: Double

--     (Just t) = tValue mu0 mu s n
--     pLow     = pValueLowerTailed t n
--     pUpper   = pValueUpperTailed t n
--     pTwo     = pValueTwoTailed t n
--     tLow     = tTestLowerTailed mu0 mu s n alpha
--     tUpper   = tTestUpperTailed mu0 mu s n alpha
--     tTwo     = tTestTwoTailed mu0 mu s n alpha

-- print $ "t value = " ++ show t

-- print $ "p lower tail value = " ++ show pLow
-- print $ "p upper tail value = " ++ show pUpper
-- print $ "p two tail value = " ++ show pTwo

-- print $ "t-test lower tail = " ++ show tLow
-- print $ "t-test upper tail = " ++ show tUpper
-- print $ "t-test two tail = " ++ show tTwo
-- "t value = -17.530772943598347"
-- "p lower tail value = 1.72194932735935e-13"
-- "p upper tail value = 0.9999999999998278"
-- "p two tail value = 3.4438986547187e-13"
-- "t-test lower tail = Just False"
-- "t-test upper tail = Just True"
-- "t-test two tail = Just False"

-- EXPLANATION OF t-test
-- There are two kinds of hypotheses for a one sample t-test, the null hypothesis 
-- and the alternative hypothesis. The alternative hypothesis assumes that some 
-- difference exists between the true mean (μ) and the comparison value (m0), 
-- whereas the null hypothesis assumes that no difference exists. 
-- The purpose of the one sample t-test is to determine if the null hypothesis 
-- should be rejected, given the sample data. The alternative hypothesis can
-- assume one of three forms depending on the question being asked. If the goal 
-- is to measure any difference, regardless of direction, a two-tailed hypothesis 
-- is used. 

-- The null hypothesis (H0) assumes that the difference between the true mean 
-- (μ) and the comparison value (m0) is equal to zero.

-- The two-tailed alternative hypothesis (H1) assumes that the difference between 
-- the true mean (μ) and the comparison value (m0) is not equal to zero.

-- The upper-tailed alternative hypothesis (H1) assumes that the true mean (μ) 
-- of the sample is greater than the comparison value (m0).

-- The lower-tailed alternative hypothesis (H1) assumes that the true mean (μ)
-- of the sample is less than the comparison value (m0).

-- The p-value gives the probability of observing the test results under the 
-- null hypothesis. The lower the p-value, the lower the probability of 
-- obtaining a result like the one that was observed if the null hypothesis 
-- was true. Thus, a low p-value indicates decreased support for the null hypothesis. 
-- However, the possibility that the null hypothesis is true and that we simply
-- obtained a very rare result can never be ruled out completely. The cutoff 
-- value for determining statistical significance is ultimately decided on by 
-- the researcher, but usually a value of .05 or less is chosen. This 
-- corresponds to a 5% (or less) chance of obtaining a result like the one 
-- that was observed if the null hypothesis was true.

-- On the meaning of the p-value https://www.statsdirect.com/help/basics/p_values.htm:
-- "If your P value is less than the chosen significance level then you reject 
-- the null hypothesis i.e. accept that your sample gives reasonable evidence 
-- to support the alternative hypothesis."
-- => because we want to return (Just True) for our t-tests in case the test 
-- accepts the null hypothesis, we need to check if the computed p value is
-- larger than the significance level (alpha)

-- MORE ON THE MEANING OF the p-value:
--     A small p-value (typically ≤ 0.05) indicates strong evidence against the 
--        null hypothesis, so you reject the null hypothesis.

--     A large p-value (> 0.05) indicates weak evidence against the null 
--        hypothesis, so you fail to reject the null hypothesis.
--------------------------------------------------------------------------------

data TTestTail = LowerTail | UpperTail | TwoTail deriving Eq

tTestSamples :: TTestTail  -- ^ type of t-test
             -> Double     -- ^ expected mean
             -> Double     -- ^ confidence
             -> [Double]   -- ^ samples
             -> Maybe Bool -- ^ Just True in case H0 (null hypothesis) is accepted
tTestSamples tt mu0 alpha xs
    | isNothing t = Nothing
    | otherwise   = Just $ p > alpha  -- ACCEPT H0 if p > alpha
    
  where
    -- get number of samples
    n  = List.length xs     
    -- compute mean of samples
    mu = Utils.Stats.mean xs
    -- compute standard deviation of samples
    s  = Utils.Stats.std xs
    -- compute t-value (t-statistics)
    t  = tValue mu0 mu s n
    -- compute p (proabilitiy) value for t-value: the p-value gives us the 
    -- probability that we would observe a test statistic t-value if the means 
    -- are really really mu0
    p  = case tt of
        LowerTail ->  pValueLowerTailed (fromJust t) n
        UpperTail -> pValueUpperTailed (fromJust t) n
        TwoTail   -> pValueTwoTailed (fromJust t) n

pValueTwoTailed :: Double -- ^ t-value
                -> Int    -- ^ number of samples
                -> Double -- ^ p-value for 1 sided test
pValueTwoTailed t n = p
  where
    degFree = fromIntegral (n - 1)
    tDist   = StudT.studentT degFree
    tAbs    = abs t
    p       = 2 * Distr.cumulative tDist (-tAbs)

-- THIS IS VALIDATED AND IS CORRECT: https://en.wikibooks.org/wiki/Statistics/Testing_Data/t-tests
--    pValue1Sided -17.5 20 == 1.72194932735935e-13
pValueLowerTailed :: Double -- ^ t-value
                  -> Int    -- ^ number of samples
                  -> Double -- ^ p-value for 1 sided test
pValueLowerTailed t n = p
  where
    degFree = fromIntegral (n - 1)
    tDist   = StudT.studentT degFree
    p       = Distr.cumulative tDist t -- LOWER TAILED: p = Pr(T < t)

pValueUpperTailed :: Double -- ^ t-value
                  -> Int    -- ^ number of samples
                  -> Double -- ^ p-value for 1 sided test
pValueUpperTailed t n = p
  where
    degFree = fromIntegral (n - 1)
    tDist   = StudT.studentT degFree
    p       = Distr.cumulative tDist (-t) -- UPPER TAILED: p = Pr(T > t) => negate t !!

-- note that t-value is undefined in case of 0 variance, all samples are the same
-- THIS IS VALIDATED AND IS CORRECT: https://en.wikibooks.org/wiki/Statistics/Testing_Data/t-tests
--    tValue 60 50.2 2.5 20 == -17.5
tValue :: Double        -- ^ expected mean
       -> Double        -- ^ actual mean of the samples
       -> Double        -- ^ standard deviation of the samples
       -> Int           -- ^ number of samples
       -> Maybe Double  -- ^ Just t-value or Nothing in case of 0 variance
tValue mu0 mu s n
    | s == 0    = Nothing
    | otherwise = Just t
  where
    t = (mu - mu0) / (s / sqrt (fromIntegral n))

avgTest :: [Double]
        -> Double
        -> Double
        -> Bool
avgTest ys mu0 eps 
    = abs (mu - mu0) <= eps
  where
    mu = Utils.Stats.mean ys

expCDF :: Double -> Double -> Double
expCDF lambda = Distr.cumulative exptDist
  where
    exptDist = Exp.exponential lambda

-- statistics package obviously provides mean and variance implementations
-- but they don't support simple lists ...
mean :: [Double] -> Double
mean xs = Sample.mean (Vect.fromList xs :: Sample)

-- statistics package obviously provides mean and variance implementations
-- but they don't support simple lists ...
_median :: [Double] -> Double
_median xs = Sample.mean (Vect.fromList xs :: Sample)


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

nearlyEqual :: Double -> Double -> Double -> Bool
nearlyEqual a b epsilon 
    | a == b 
      = True -- shortcut, handles infinities
    | (a == 0 || b == 0 || diff < minValue) 
      -- a or b is zero or both are extremely close to it
      -- relative error is less meaningful here
      =  diff < (epsilon * minValue)
    | otherwise 
      -- use relative error
      = diff / (absA + absB) < epsilon 
  where
    absA = abs a
    absB = abs b
    diff = abs (a - b)

minValue :: (RealFloat a) => a
minValue = x
  where n = floatDigits x
        b = floatRadix x
        (l, _) = floatRange x
        x = encodeFloat (b^n - 1) (l - n - 1)

{-
histogram :: [Double] -> Int -> [(Double, Int)]
histogram xs bins = Vect.toList histVect
  where
    xsVect = Vect.fromList xs
    (intervals, samples) = Hist.histogram bins xsVect
    histVect = Vect.zip intervals samples
-}