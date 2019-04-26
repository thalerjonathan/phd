module Main where

import Statistics.Test.StudentT
import Statistics.Test.MannWhitneyU
import Statistics.Types
import qualified Data.Vector.Generic as Vect

main :: IO ()
main = do
  let p  = 0.05
      xs = [134, 146, 104, 119, 124, 161, 107, 83, 113, 129, 97, 123]
      ys = [70, 118, 101, 85, 107, 132, 94]

  let mwRet = mannWhitneyTwoSample xs ys p
      ttRet = ttestTwoSample xs ys p

  putStrLn $ "MannWhitney = " ++ show mwRet
  putStrLn $ "two sample t-test = " ++ show ttRet

ttestTwoSample :: [Double]    -- ^ first samples
               -> [Double]    -- ^ second samples
               -> Double      -- ^ p value
               -> Maybe Bool  -- ^ Just True in case the means are the same
ttestTwoSample xs ys p = do
  let xsVect = Vect.fromList xs :: Sample
  let ysVect = Vect.fromList ys :: Sample
  ret <- studentTTest SamplesDiffer xsVect ysVect

  let sig = isSignificant (mkPValue p) ret
  return (NotSignificant == sig)

mannWhitneyTwoSample :: [Double]
                     -> [Double]
                     -> Double
                     -> Maybe Bool
mannWhitneyTwoSample xs ys p = do
  let xsVect = Vect.fromList xs
  let ysVect = Vect.fromList ys
  ret <- mannWhitneyUtest SamplesDiffer (mkPValue p) xsVect ysVect
  return (NotSignificant == ret)

mannWhitneyOneSample :: [Double]
                     -> Double
                     -> Double
                     -> Maybe Bool
mannWhitneyOneSample xs mu = mannWhitneyTwoSample xs (replicate (length xs) mu)