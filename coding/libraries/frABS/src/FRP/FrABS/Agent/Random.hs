module FRP.FrABS.Agent.Random (
    agentRandom,
    agentRandomM,
    
    agentRandomRange,
    agentRandomRanges,
    agentRandomBoolProb,
    agentRandomBoolProbM,
    agentRandomSplit,
    agentRandomPick,
    agentRandomPickM,
    agentRandomPicks,
    agentRandomPickM,

    randomBool,
    randomBoolM,
    randomExp,
    randomExpM,

    avoid
  ) where

import FRP.FrABS.Agent.Agent

import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

-------------------------------------------------------------------------------
-- RUNNING AGENT RANDOM-FUNCTION
-------------------------------------------------------------------------------
-- NOTE: beware of a = AgentOut (randomly manipulating AgentOut) because one will end up with 2 versions of AgentOut which need to be merged
agentRandom :: Rand StdGen a -> AgentOut s m e -> (a, AgentOut s m e)
agentRandom f a = (ret, a')
    where
        g = aoRng a
        (ret, g') = runRand f g
        a' = a {aoRng = g'}

agentRandomM :: Rand StdGen a -> State (AgentOut s m e) a
agentRandomM f = state (runAgentRandomMAux f)
    where
        runAgentRandomMAux :: Rand StdGen a -> AgentOut s m e -> (a, AgentOut s m e)
        runAgentRandomMAux f ao = agentRandom f ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- AGENT RANDOM-FUNCTIONS
-------------------------------------------------------------------------------
agentRandomRange :: (Random a) => (a, a) -> AgentOut s m e -> (a, AgentOut s m e)
agentRandomRange r a = agentRandom (getRandomR r) a 

agentRandomRanges :: (Random a) => (a, a) -> Int -> AgentOut s m e -> ([a], AgentOut s m e)
agentRandomRanges r n a = agentRandom blub a 
    where
        blub = do
                infRand <- getRandomRs r
                let nRand = take n infRand
                return nRand

agentRandomBoolProb :: Double -> AgentOut s m e -> (Bool, AgentOut s m e)
agentRandomBoolProb p ao = agentRandom (randomBoolM p) ao

agentRandomBoolProbM :: Double -> State (AgentOut s m e) Bool
agentRandomBoolProbM p = state agentRandomBoolProbMAux 
    where
        agentRandomBoolProbMAux :: (AgentOut s m e) -> (Bool, AgentOut s m e)
        agentRandomBoolProbMAux ao = agentRandomBoolProb p ao

agentRandomSplit :: AgentOut s m e -> (StdGen, AgentOut s m e)
agentRandomSplit a = agentRandom getSplit a 

agentRandomPick :: [a] -> AgentOut s m e -> (a, AgentOut s m e)
agentRandomPick xs a 
    | null xs = error "cannot draw single random element from empty list"
    | otherwise = (randElem, a')
    where
        cellCount = length xs
        (randIdx, a') = agentRandomRange (0, cellCount - 1) a 
        randElem = xs !! randIdx

agentRandomPickM :: [a] -> State (AgentOut s m e) a
agentRandomPickM xs = state (\ao -> agentRandomPick xs ao)

agentRandomPicks :: [a] -> Int -> AgentOut s m e -> ([a], AgentOut s m e)
agentRandomPicks xs n a 
    | null xs = error "cannot draw multiple random elements from empty list"
    | otherwise = (randElems, a')
    where
        cellCount = length xs
        (randIndices, a') = agentRandomRanges (0, cellCount - 1) n a 
        randElems = foldr (\idx acc -> (xs !! idx) : acc) [] randIndices  

agentRandomPicksM :: [a] -> Int -> State (AgentOut s m e) [a]
agentRandomPicksM xs n = state (\ao -> agentRandomPicks xs n ao)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- RANDOM-MONAD FUNCTIONS
-------------------------------------------------------------------------------
randomBoolM :: (RandomGen g) => Double -> Rand g Bool
randomBoolM p = getRandomR (0.0, 1.0) >>= (\r -> return $ p >= r)

randomBool :: (RandomGen g) => g -> Double -> (Bool, g)
randomBool g p = runRand (randomBoolM p) g

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
randomExpM :: (RandomGen g) => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))

randomExp :: (RandomGen g) => g -> Double -> (Double, g)
randomExp g lambda = runRand (randomExpM lambda) g

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
avoid x = 
    do
        r <- getRandom
        if (r == x) then
            avoid x
            else
                return r
-------------------------------------------------------------------------------