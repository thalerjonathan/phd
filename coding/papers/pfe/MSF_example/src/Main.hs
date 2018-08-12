{-# LANGUAGE Arrows                #-}

module Main where 

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.MonadicStreamFunction
import FRP.BearRiver

type FallingMassStack g = (StateT Int (RandT g IO))
type FallingMassMSF g = SF (FallingMassStack g) () Double

main :: IO ()
main = do
  let g0  = mkStdGen 42
      s0  = 0
      msf = fallingMassMSF 0 100

  runMSF g0 s0 msf

runMSF :: RandomGen g
       => g
       -> Int
       -> FallingMassMSF g
       -> IO ()
runMSF g s msf = do
  let msfReaderT = unMSF msf ()
      msfStateT  = runReaderT msfReaderT 0.1
      msfRand    = runStateT msfStateT s
      msfIO      = runRandT msfRand g

  (((p, msf'), s'), g') <- msfIO
  
  when (p > 0) (runMSF g' s' msf')

fallingMassMSF :: RandomGen g
               => Double -> Double -> FallingMassMSF g
fallingMassMSF v0 p0 = proc _ -> do
  r <- arrM_ (lift $ lift $ getRandomR (0, 9.81)) -< ()

  v <- arr (+v0) <<< integral -< (-r)
  p <- arr (+p0) <<< integral -< v

  arrM_ (lift $ modify (+1)) -< ()

  if p > 0
    then returnA -< p
    else do
      s <- arrM_ (lift get) -< ()
      arrM (liftIO . putStrLn) -< "hit floor with v " ++ show v ++ " after " ++ show s ++ " steps"
      returnA -< p