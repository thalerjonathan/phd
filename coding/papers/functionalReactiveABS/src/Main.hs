module Main where

import System.IO

import SIRS.RunSIRS
-- import FrABS.YampaSeqTest

main :: IO ()
main = do
            hSetBuffering stdin NoBuffering
            hSetBuffering stderr NoBuffering
            runSIRSWithRendering -- testParEmbed -- runSIRSWithRendering --testRunSF