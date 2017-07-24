{-# LANGUAGE Arrows #-}
module FRP.FrABS.Environment.Definitions (
    EnvironmentBehaviour,
    EnvironmentMonadicBehaviour,

    EnvironmentCollapsing,

    environmentMonadic
  ) where

import FRP.Yampa

import Control.Monad.Trans.State

type EnvironmentBehaviour e = SF e e
type EnvironmentCollapsing e = ([e] -> e)

type EnvironmentMonadicBehaviour e = (Double -> State e ())

environmentMonadic :: EnvironmentMonadicBehaviour e -> EnvironmentBehaviour e
environmentMonadic f = proc e ->
    do
        t <- time -< 0

        let (_, e') = runState (f t) e

        returnA -< e'
