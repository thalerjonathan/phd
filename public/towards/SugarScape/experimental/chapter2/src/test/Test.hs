module Main where

import System.Random

import Test.Tasty
--import Test.Tasty.QuickCheck as QC

--import SugarScape.Discrete 
--import SugarScape.Model

import Agent
import Environment
import Simulation
--import StatsUtils

-- clear & stack test --test-arguments="--quickcheck-tests=100 --quickcheck-replay="

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  --let g = mkStdGen seed
  --setStdGen g
  g <- newStdGen
  
{-
  let xs = [219.9468438538206
            , 202.1627906976744
            , 205.74418604651163 
            , 198.2890365448505
            , 205.99003322259136
            , 205.57142857142858 
            , 203.1860465116279
            , 202.77408637873754
            , 200.62458471760797
            , 204.48504983388705
            , 208.156146179402
            , 196.58139534883722
            , 206.09634551495017
            , 196.9435215946844
            , 206.49169435215947
            , 204.1063122923588
            , 207.66445182724252
            , 216.734219269103
            , 214.82059800664453
            , 204.7375415282392
            , 203.64119601328903
            , 201.6013289036545
            , 192.10963455149502
            , 198.55149501661128
            , 202.58471760797343
            , 211.54817275747507
            , 196.3687707641196 
            , 205.51162790697674
            , 204.19601328903656
            , 216.35215946843854 
            , 203.41860465116278
            , 198.3953488372093
            , 208.91029900332225
            , 211.95016611295682
            , 214.42857142857142 
            , 190.80730897009965 
            , 200.3156146179402 
            , 195.27242524916943 
            , 203.29900332225913 
            , 201.3654485049834 
            , 207.34883720930233 
            , 195.68106312292358 
            , 213.33554817275748 
            , 194.78737541528238 
            , 200.1029900332226 
            , 213.14617940199335 
            , 219.531561461794 
            , 201.265780730897 
            , 213.32558139534885]

  print $ tTest "carrying caps" xs 200 0.05

  --print $ tTest "test1" [1..10] 5.5 0.05
  --print $ tTest "test2" [1..10] 0.0 0.05
-}

{-
  putStrLn ""
  x <- generate arbitrary :: IO (Discrete2d SugEnvCell)
  print x
-}

  let sugarScapeTests = testGroup "SugarScape Tests" 
                              [ 
                              agentTests g, 
                              envTests g
                              --, agentTests g  
                              --simTests g
                              ]

  defaultMain sugarScapeTests