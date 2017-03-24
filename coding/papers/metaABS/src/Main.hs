module Main where

import SIRS.RunSIRS
import Segregation.SegregationRun
import MetaABS.MetaABSRun

main :: IO ()
main = runMetaABSStepsAndPrint -- runSIRSWithRendering -- runSegWithRendering -- runSegStepsAndRender