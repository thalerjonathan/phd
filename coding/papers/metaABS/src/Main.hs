module Main where

import SIRS.RunSIRS
import Segregation.RunSegregation

main :: IO ()
main = runSegStepsAndRender -- runSIRSWithRendering -- runSegWithRendering -- runSegStepsAndRender