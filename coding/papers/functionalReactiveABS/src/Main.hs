module Main where

import System.IO

import SIRS.RunSIRS
import Segregation.RunSegregation

-- import FrABS.YampaSeqTest

main :: IO ()
main = do
            hSetBuffering stdin NoBuffering
            hSetBuffering stderr NoBuffering
            runSegWithRendering -- testParEmbed -- testSeqEmbed -- testParEmbed --testRunSF -- runSIRSWithRendering