{-# LANGUAGE Arrows     #-}
module SIRDunai 
  ( runSIRDunai
  , runSIRDunaiUntil
  ) where

import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.Trans.MSF.Random
import           FRP.BearRiver

import SIR
 
type SIRMonad g   = Rand g
type SIRAgent g   = SF (SIRMonad g) [SIRState] SIRState

type SimSF g = SF (SIRMonad g) () [SIRState]

runSIRDunai :: RandomGen g 
            => SIRSimCtx g
            -> [(Double, Double, Double)]
runSIRDunai simCtx
    = runSIRDunaiUntil g t dt as cr inf dur
  where
    t  = syCtxTimeLimit simCtx
    dt = syCtxTimeDelta simCtx
    g  = syCtxRng simCtx

    s = syCtxSusceptible simCtx
    i = syCtxInfected simCtx
    r = syCtxRecovered simCtx

    cr  = syCtxContactRate simCtx
    inf = syCtxInfectivity simCtx
    dur = syCtxIllnessDur simCtx

    as = initAgents s i r

runSIRDunaiUntil :: RandomGen g
                 => g
                 -> Time
                 -> DTime
                 -> [SIRState]
                 -> Double
                 -> Double
                 -> Double
                 -> [(Double, Double, Double)]
runSIRDunaiUntil g0 tMax dt as0 cr inf dur
    = reverse $ runSIRDunaiUntilAux 0 g0 (simulationStep asSfs as0) [] 
  where
    asSfs = map (sirAgent cr inf dur) as0

    runSIRDunaiUntilAux :: RandomGen g
                        => Time
                        -> g
                        -> SF (SIRMonad g) () [SIRState]
                        -> [(Double, Double, Double)]
                        -> [(Double, Double, Double)]
    runSIRDunaiUntilAux t g sf acc 
        | t >= tMax = acc
        | otherwise = runSIRDunaiUntilAux t' g' sf' acc'
      where
        sfReader        = unMSF sf ()
        sfRand          = runReaderT sfReader dt
        ((as, sf'), g') = runRand sfRand g
        aggr            = sirAggregate as
        
        t'   = t + dt
        acc' = aggr : acc

simulationStep :: RandomGen g
               => [SIRAgent g]
               -> [SIRState]
               -> SF (SIRMonad g) () [SIRState]
simulationStep sfs as = MSF $ \_ -> do
  --let (sfs, coords) = unzip sfsCoords 

  -- run all agents sequentially but keep the environment
  -- read-only: it is shared as input with all agents
  -- and thus cannot be changed by the agents themselves
  -- run agents sequentially but with shared, read-only environment
  ret <- mapM (`unMSF` as) sfs
  -- construct new environment from all agent outputs for next step
  let (as', sfs') = unzip ret
      cont = simulationStep sfs' as'
  return (as', cont)

sirAgent :: RandomGen g 
         => Double
         -> Double
         -> Double
         -> SIRState 
         -> SIRAgent g
sirAgent cr inf dur Susceptible = susceptibleAgent cr inf dur
sirAgent _  _   dur Infected    = infectedAgent dur
sirAgent _  _   _   Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g 
                 => Double
                 -> Double
                 -> Double 
                 -> SIRAgent g
susceptibleAgent cr inf dur
    = switch 
      -- delay the switching by 1 step, otherwise could
      -- make the transition from Susceptible to Recovered within time-step
      --(susceptible >>> iPre (Susceptible, NoEvent))
      susceptible
      (const $ infectedAgent dur)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) [SIRState] (SIRState, Event ())
    susceptible = proc as -> do
      makeContact <- occasionally (1 / cr) () -< ()

      if not $ isEvent makeContact 
        then returnA -< (Susceptible, NoEvent)
        else (do
          s <- drawRandomElemS -< as
          case s of
            Infected -> do
              infected <- arrM_ (lift $ randomBoolM inf) -< ()
              if infected 
                then returnA -< (Infected, Event ())
                else returnA -< (Susceptible, NoEvent)
            _       -> returnA -< (Susceptible, NoEvent))

infectedAgent :: RandomGen g 
              => Double
              -> SIRAgent g
infectedAgent dur
    = switch
      -- delay the switching by 1 step, otherwise could
      -- make the transition from Susceptible to Recovered within time-step
      --(infected >>> iPre (Infected, NoEvent))
      infected
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) [SIRState] (SIRState, Event ())
    infected = proc _ -> do
      recovered <- occasionally dur () -< ()
      if isEvent recovered
        then returnA -< (Recovered, Event ())
        else returnA -< (Infected, NoEvent)

recoveredAgent :: RandomGen g => SIRAgent g
recoveredAgent = arr (const Recovered) 

drawRandomElemS :: MonadRandom m => SF m [a] a
drawRandomElemS = proc as -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  let len = length as
  let idx = fromIntegral len * r
  let a =  as !! floor idx
  returnA -< a

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)