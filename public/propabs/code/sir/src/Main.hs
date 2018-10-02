module Main where

import System.Random

import Test.Tasty

import SIRYampaTests

-- to generate html from coverage use hpc:
-- hpc markup --hpcdir=/home/io.nathan/phd/coding/reports/verificationABS/Testing/.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/hpc SIRABS.tix

main :: IO ()
main = do
  g <- getStdGen
  defaultMain $ sirYampaTests g