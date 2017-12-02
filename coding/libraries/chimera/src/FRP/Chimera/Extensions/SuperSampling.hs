module FRP.Chimera.Extensions.SuperSampling
  (
    superSampling
  ) where

import FRP.Yampa.InternalCore

-- TODO: implement different samling-strategies: random noise, triangle, uniform, predefined sampledistances

superSampling :: Int -> SF a b -> SF a [b]
superSampling n sf0 = SF { sfTF = tf0 }
  where
    -- NOTE: no supersampling at time 0
    tf0 a0 = (tfCont, [b0])
      where
        (sf', b0) = sfTF sf0 a0
        tfCont = superSamplingAux sf'

    superSamplingAux sf' = SF' tf
      where
        tf dt a = (tf', bs)
          where
            (sf'', bs) = superSampleRun n dt sf' a
            tf' = superSamplingAux sf''

    superSampleRun :: Int -> DTime -> SF' a b -> a -> (SF' a b, [b])
    superSampleRun n dt sf a 
        | n <= 1 = superSampleMulti 1 dt sf a []
        | otherwise = (sf', reverse bs)  -- NOTE: need to reverse because need to respect order, use of accumulator reverses them initially
      where
        superDt = dt / fromIntegral n
        (sf', bs) = superSampleMulti n superDt sf a []

    superSampleMulti :: Int -> DTime -> SF' a b -> a -> [b] -> (SF' a b, [b])
    superSampleMulti 0 _ sf _ acc = (sf, acc)
    superSampleMulti n dt sf a acc = superSampleMulti (n-1) dt sf' a (b:acc) 
      where
        (sf', b) = sfTF' sf dt a
-------------------------------------------------------------------------------