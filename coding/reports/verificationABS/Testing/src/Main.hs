module Main where

import System.Random

import ABS
import SD
import SIR

main :: IO ()
main = do
  g <- getStdGen
  
  let popSize = 100 :: Double
  let infCount = 1   :: Double

  let sdDyns  = runSD popSize infCount 150 0.1
  let absDyns = runABS g (floor popSize) (floor infCount) 150 0.01

  let (sdSus, sdInf, sdRec)    = unzip3 sdDyns
  let (absSus, absInf, absRec) = unzip3 absDyns

  let sdfilename  = "sd_" ++ show popSize ++ ".m"
  let absfilename = "abs_" ++ show popSize ++ ".m"

  writeAggregatesToFile sdfilename sdDyns
  writeAggregatesToFile absfilename absDyns