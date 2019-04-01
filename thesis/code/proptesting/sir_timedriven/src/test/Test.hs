module Main where

import Test.Tasty

import SIRTests

-- clear & stack test --test-arguments="--quickcheck-tests=100 --quickcheck-replay=67991"
-- clear & stack test --test-arguments="--quickcheck-tests=100" &> testout

main :: IO ()
main = defaultMain sirPropTests