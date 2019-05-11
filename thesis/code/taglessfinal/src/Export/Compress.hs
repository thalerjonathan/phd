module Export.Compress where

--------------------------------------------------------------------------------
-- AGGREGATION UTILS
--------------------------------------------------------------------------------
compressOutput :: [(Double, (Int, Int, Int))] -> [(Double, (Int, Int, Int))]
compressOutput = foldr compressOutputAux []
  where
    compressOutputAux :: (Double, (Int, Int, Int))
                      -> [(Double, (Int, Int, Int))]
                      -> [(Double, (Int, Int, Int))]
    compressOutputAux ts [] = [ts]
    compressOutputAux (t, s) ((t', s') : acc) 
      | t == t'   = (t, s) : acc
      | otherwise = (t, s) : (t', s') : acc