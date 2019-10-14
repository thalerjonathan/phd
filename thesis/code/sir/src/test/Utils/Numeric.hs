module Utils.Numeric (
    compareDouble
  ) where

compareDouble :: Double -> Double -> Double -> Bool
compareDouble a b eps 
    | a == b 
      = True -- shortcut, handles infinities
    | a == 0 || b == 0 || diff < minValue
      -- a or b is zero or both are extremely close to it
      -- relative error is less meaningful here
      =  diff < (eps * minValue)
    | otherwise 
      -- use relative error
      = diff / (absA + absB) < eps 
  where
    absA = abs a
    absB = abs b
    diff = abs (a - b)

minValue :: (RealFloat a) => a
minValue = x
  where n = floatDigits x
        b = floatRadix x
        (l, _) = floatRange x
        x = encodeFloat (b^n - 1) (l - n - 1)