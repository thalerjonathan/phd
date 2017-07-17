module FRP.FrABS.Environment.Spatial (
    EnvironmentWrapping (..)
  ) where

data EnvironmentWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth
-- newtype EnvironmentSpatial2DDimension d = (Num d, Num d)

{-
class EnvSpatial2d e d where
    agentCoord :: (Num d) => AgentId -> e -> (d, d)
    updateAgentCoord :: (Num d) => AgentId -> (d, d) -> e -> e
    environmentDimensions :: (Num d) => e -> (d, d)
-}