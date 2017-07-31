module FRP.FrABS.Environment.Spatial (
    EnvironmentWrapping (..)
  ) where

data EnvironmentWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth
-- newtype EnvironmentSpatial2DDimension d = (Num d, Num d)