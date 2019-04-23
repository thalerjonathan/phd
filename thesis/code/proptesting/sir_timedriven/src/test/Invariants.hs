{-# LANGUAGE InstanceSigs #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SIRGenerators

-- import Debug.Trace 

-- --quickcheck-replay=557780
-- --quickcheck-tests=1000
-- --quickcheck-verbose
-- clear & stack test sir-time:sir-invariants-test --test-arguments="--quickcheck-tests=1000"

main :: IO ()
main = do
  let t = testGroup "SIR Invariants Tests" 
          [ 
            QC.testProperty "SIR imulation invariants" prop_sir_invariants
          ]

  defaultMain t

--------------------------------------------------------------------------------
-- SIMULATION INVARIANTS
--------------------------------------------------------------------------------
prop_sir_invariants :: Positive Double -- ^ Random beta, contact rate
                    -> Positive Double -- ^ Random gamma, infectivity
                    -> Positive Double -- ^ Random delta, illness duration
                    -> Property
prop_sir_invariants (Positive cor) (Positive inf) (Positive ild) = property $ do
  -- generate population with size of up to 1000
  as <- resize 1000 (listOf genSIRState)
  -- total agent count
  let n = length as

  -- run for inifinite time
  let t  = 0
      dt = 0.1
  ret <- genSIR t dt as cor inf ild

  -- after a finite number of steps SIR will reach equilibrium, when there
  -- are no more infected agents. WARNING: this could be a potentially non-
  -- terminating computation but a correct SIR implementation will always
  -- lead to a termination of this 
  let equilibriumData = takeWhile ((>0).snd3) ret

  return (sirInvariants n equilibriumData)

sirInvariants :: Int -> [(Int, Int, Int)] -> Bool
sirInvariants n sirs = aConst && susDec && recInc && infInv
  where
    (ss, _, rs) = unzip3 sirs

    -- 2. number of agents N stays constant in each step
    aConst = all agentCountInv sirs
    -- 3. number of susceptible S is monotonic decreasing
    susDec = mono (>=) ss
    -- 4. number of recovered R is monotonic increasing
    recInc = mono (<=)  rs
    -- 5. number of infected I = N - (S + R)
    infInv = all infectedInv sirs

    agentCountInv :: (Int, Int, Int) -> Bool
    agentCountInv (s,i,r) = s + i + r == n

    infectedInv :: (Int, Int, Int) -> Bool
    infectedInv (s,i,r) = i == n - (s + r)

    mono :: (Ord a, Num a) => (a -> a -> Bool) -> [a] -> Bool
    mono f xs = all (uncurry f) (pairs xs)

    pairs :: [a] -> [(a,a)]
    pairs xs = zip xs (tail xs)

--------------------------------------------------------------------------------
-- UTILS
--------------------------------------------------------------------------------
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b