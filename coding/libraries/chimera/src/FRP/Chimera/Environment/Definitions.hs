{-# LANGUAGE Arrows #-}
module FRP.Chimera.Environment.Definitions 
  (
    EnvironmentBehaviour
  , EnvironmentMonadicBehaviour

  , EnvironmentFolding

  , environmentMonadic
  ) where

import Control.Monad.Trans.State

import FRP.Yampa

type EnvironmentBehaviour e         = SF e e
type EnvironmentFolding e           = ([e] -> e)

type EnvironmentMonadicBehaviour e  = (Double -> State e ())

environmentMonadic :: EnvironmentMonadicBehaviour e -> EnvironmentBehaviour e
environmentMonadic f = proc e -> do
  t <- time -< ()
  let (_, e') = runState (f t) e
  returnA -< e'
