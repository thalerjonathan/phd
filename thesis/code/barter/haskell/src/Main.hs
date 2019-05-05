module Main where

import Control.Monad.Random

import BarterEconomy

-- NOTE: to get the call stack in case of an exception (e.g. !! access)
-- clear & stack build --profile
-- clear & stack exec -- haskell-barter +RTS -xc

main :: IO ()
main = do
  putStr "Starting simulation..."
  ret <- reverse . take 10 <$> evalRandIO start
  putStrLn "finished"
  print ret
