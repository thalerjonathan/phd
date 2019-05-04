module Main where

import Control.Monad.Random

import BarterEconomy

main :: IO ()
main = do
  evalRandIO start
  putStrLn "Finished"
