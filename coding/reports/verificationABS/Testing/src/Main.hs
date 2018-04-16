module Main where

import System.Random

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import ABSFeedbackTests
import ABSDataTests

-- to generate html from coverage use hpc:
-- hpc markup --hpcdir=/home/io.nathan/phd/coding/reports/verificationABS/Testing/.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/hpc SIRABS.tix

main :: IO ()
main = do
  g <- getStdGen

  let tests = [absDataTests g] --[ absFeedbackTests g, absDataTests g ]

  defaultMain $ testGroup "SIR ABS Tests" tests