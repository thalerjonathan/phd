module Main where

import Model.Model
import Test.QuickCheck

repls :: Int
repls = 10

confidence :: Double
confidence = 0.95

-- clear & stack test SugarScape:sugarscape-model-test

main :: IO ()
main = do
 

  let tests = [ ("Disease Dynamics All Recover", prop_disease_allrecover)
              , ("Disease Dynamics Minority Recover", prop_disease_norecover)
              , ("Trading Dynamics", prop_trading)
              , ("Cultural Dynamics", prop_culture)
              , ("Inheritance Gini", prop_gini repls confidence)
              , ("Carrying Capacity", prop_carrying repls confidence)
              , ("Terracing", prop_terracing repls confidence)
              , ("Wealth Distribution", prop_wealth repls confidence)
              ]

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"
  putStrLn "Running Model Tests..."

  mapM_ (\(tName, t) -> do
    putStrLn ""
    putStrLn $ "Testing " ++ tName ++ " ..."
    quickCheckWith modelTestingArgs t) tests

  putStrLn ""
  putStrLn "Running Model Tests finished."
  putStrLn "--------------------------------------------------------------------------------"

modelTestingArgs :: Args
modelTestingArgs = stdArgs { maxSuccess = 10      -- number successful tests
                           , maxFailPercent = 100 -- number of maximum failed tests
                           , maxShrinks = 0       -- NO SHRINKS, doesn't make sense in the model tests
                            --, replay = Just (mkQCGen 42, 0) -- use to replay reproducible
                           }