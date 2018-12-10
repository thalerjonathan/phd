module SugarScape.Simulation
  ( SimulationState (..)
  , SimStepOut
  , AgentObservable

  , initSimulation
  , initSimulationOpt
  , initSimulationRng
  , initSimulationSeed

  , simulationStep
  , simStepSF

  , mkSimState
  , simulateUntil

  , sugarScapeTimeDelta
  ) where

import qualified Data.Map as Map
import Data.Maybe
import System.Random

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Discrete 
import SugarScape.Environment
import SugarScape.Model
import SugarScape.Init
import SugarScape.Random

type AgentObservable o = (AgentId, o)
type SimStepOut        = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

-- NOTE: strictness on those fields, otherwise space-leak 
-- (memory consumption increases throughout run-time and execution gets slower and slower)
data SimulationState g = SimulationState 
  { simSf       :: SF (SugAgentMonadT g) SugEnvironment ([AgentObservable SugAgentObservable], SugEnvironment)
  , simAbsState :: !ABSState 
  , simEnv      :: !SugEnvironment
  , simRng      :: !g
  , simSteps    :: !Int
  }

-- sugarscape is stepped with a time-delta of 1.0
sugarScapeTimeDelta :: DTime
sugarScapeTimeDelta = 1.0

initSimulation :: SugarScapeParams
               -> IO (SimulationState StdGen, SugEnvironment)
initSimulation params = do
  g0 <- newStdGen
  return $ initSimulationRng g0 params

initSimulationOpt :: Maybe Int
                  -> SugarScapeParams
                  -> IO (SimulationState StdGen, SugEnvironment)
initSimulationOpt Nothing     params = initSimulation params
initSimulationOpt (Just seed) params = return $ initSimulationSeed seed params

initSimulationSeed :: Int
                   -> SugarScapeParams
                   -> (SimulationState StdGen, SugEnvironment)
initSimulationSeed seed = initSimulationRng g0
  where
    g0 = mkStdGen seed

initSimulationRng :: RandomGen g
                   => g
                   -> SugarScapeParams
                   -> (SimulationState g, SugEnvironment)
initSimulationRng g0 params = (initSimState, initEnv)
  where
    -- initial agents and environment data
    ((initAs, initEnv), g) = runRand (createSugarScape params) g0
    -- initial simulation state
    (initAis, initSfs)     = unzip initAs
    -- initial simulation state
    initSimState           = mkSimState 
                              (simStepSF initAis initSfs (sugEnvironment params)) 
                              (mkAbsState $ maximum initAis) initEnv g 0

simulateUntil :: RandomGen g
              => Time
              -> SimulationState g
              -> [SimStepOut]
simulateUntil tMax ss0 = simulateUntilAux ss0 []
  where
    simulateUntilAux :: SimulationState g
                     -> [SimStepOut]
                     -> [SimStepOut]
    simulateUntilAux ss acc
        | t < tMax  = simulateUntilAux ss' acc'
        | otherwise = reverse acc'
      where
        (ss', so@(t, _, _)) = simulationStep ss
        acc' = so : acc

simulationStep :: SimulationState g
               -> (SimulationState g, SimStepOut)
simulationStep ss = (ss', (t, env', out))
  where
    sf       = simSf ss
    absState = simAbsState ss
    env      = simEnv ss
    g        = simRng ss
    steps    = simSteps ss

    sfReader   = unMSF sf env
    sfAbsState = runReaderT sfReader sugarScapeTimeDelta
    sfRand     = runStateT sfAbsState absState
    ((((out, env'), sf'), absState'), g') = runRand sfRand g

    t          = absTime absState 
    absState'' = absState' { absTime = t + sugarScapeTimeDelta }
    ss'        = mkSimState sf' absState'' env' g' (steps + 1)

-- recursively resolve conflicts:
  --   a conflict is when one or more agents are on same positions, 
  --   in this case a winner is selected randomly and the remaining
  --   agents are rolled back (previous sf) and re-run with an updated
  --   environment:
  --   the updated environment is the environment before the step
  --   with the sites updated of the agents which are in no conflict / 
  --   who were selected as winners
  --   after all conflicts were resolved, create the final environment

  -- for now we just run the set of agents in conflict again without selecting
  -- a winner
type AgentConflictData g = (AgentId, SugAgentOut g, SugEnvironment, SugAgent g)

runAndResolveConflicts :: RandomGen g
                       => [AgentId]
                       -> [SugAgent g]
                       -> SugEnvironment
                       -> [(AgentId, SugAgentOut g, SugAgent g)]
                       -> ReaderT DTime (SugAgentMonadT g) ([(AgentId, SugAgentOut g, SugAgent g)], SugEnvironment)
runAndResolveConflicts ais0 sfs0 env0 finalAs = do
    sfsAis <- lift $ lift $ fisherYatesShuffleM (zip sfs0 ais0)
    
    let (sfs, ais) = unzip sfsAis

    ret <- mapM (`unMSF` env0) sfs

    let coordMap = foldr insertByCoord Map.empty (zip ais ret)
        els      = Map.elems coordMap
        (nonConflictAs, conflictAs) = foldr divide ([], []) els

        env' = foldr foldEnvironment env0 nonConflictAs

    if null conflictAs
      then return (nonConflictAs, env')
      else runAndResolveConflicts

  where
    foldEnvironment :: AgentConflictData g
                    -> SugEnvironment
                    -> SugEnvironment
    foldEnvironment (_, ao, ae, _) env = env'
      where
        obs   = fromJust $ aoObservable ao
        coord = sugObsCoord obs
        cell  = cellAt coord ae
        env'  = changeCellAt coord cell env

    divide :: [AgentConflictData g]
           -> ([AgentConflictData g], [AgentConflictData g])
           -> ([AgentConflictData g], [AgentConflictData g])
    divide [acd] (ncas, cas)        = (acd : ncas, cas)
    -- TODO: would need the old SF for the conflicting agents
    divide (acd : acds) (ncas, cas) = (acd : ncas, cas ++ acds) -- select the first to win this site and add the others to the conflicting agents,

    insertByCoord :: RandomGen g
                  => (AgentId, ((SugAgentOut g, SugEnvironment), SugAgent g))
                  -> Map.Map Discrete2dCoord [AgentConflictData g]
                  -> Map.Map Discrete2dCoord [AgentConflictData g]
    insertByCoord (aid, ((ao, aenv), asf)) acc = acc'
        -- | not $ isObservable ao = acc
        -- | otherwise             = acc'
      where
        obs   = fromJust $ aoObservable ao
        coord = sugObsCoord obs
        acc'  = Map.alter (insertWithDefault (aid, ao, aenv, asf)) coord acc

        insertWithDefault :: RandomGen g
                          => AgentConflictData g
                          -> Maybe [AgentConflictData g]
                          -> Maybe [AgentConflictData g]
        insertWithDefault v Nothing   = Just [v]
        insertWithDefault v (Just es) = Just $ v : es

simStepSF :: RandomGen g
          => [AgentId]
          -> [SugAgent g]
          -> SugAgent g
          -> SF (SugAgentMonadT g) SugEnvironment ([AgentObservable SugAgentObservable], SugEnvironment)
simStepSF ais0 sfs0 envSf0 = MSF $ \env -> do
  (as, env') <- runAndResolveConflicts ais0 sfs0 env []
  -- run environment separately after all agents
  ((_, env'), envSf') <- unMSF envSf0 env'

  let adefs  = concatMap (\(_, ao, _) -> aoCreate ao) as
      newSfs = map adBeh adefs
      newAis = map adId adefs

      obs = foldr (\(aid, ao, _) acc -> 
        if isObservable ao 
          then (aid, fromJust $ aoObservable ao) : acc  
          else acc) [] as

      (sfs', ais') = foldr (\(aid, ao, sf) acc@(accSf, accAid) -> 
        if isDead ao 
          then acc 
          else (sf : accSf, aid : accAid)) ([], []) as

      ct = simStepSF (newAis ++ ais') (newSfs ++ sfs') envSf'

  return (([], env), ct)

mkSimState :: SF (SugAgentMonadT g) SugEnvironment ([AgentObservable SugAgentObservable], SugEnvironment)
           -> ABSState
           -> SugEnvironment
           -> g
           -> Int
           -> SimulationState g
mkSimState sf absState env g steps = SimulationState 
  { simSf       = sf
  , simAbsState = absState 
  , simEnv      = env
  , simRng      = g
  , simSteps    = steps
  }