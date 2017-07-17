module FRP.FrABS.Environment.Definitions (
    EnvironmentBehaviour,
    EnvironmentCollapsing
  ) where

import FRP.Yampa

type EnvironmentBehaviour e = SF e e
type EnvironmentCollapsing e = ([e] -> e)