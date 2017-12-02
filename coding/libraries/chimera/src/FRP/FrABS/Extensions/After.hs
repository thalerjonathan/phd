module FRP.FrABS.Extensions.After
  (
    afterExp
  , afterExp'
  ) where

import Control.Monad.Random

import FRP.Yampa
import FRP.Yampa.InternalCore

import FRP.FrABS.Random.Pure

afterExp :: RandomGen g => g -> DTime -> b -> SF a (Event b)
afterExp g t b = after tExp b
  where
    (tExp, _) = randomExp g (1 / t)

-- NOTE: this is our own implementation, just for the fun of it
afterExp' :: RandomGen g => g -> DTime -> b -> SF a (Event b)
afterExp' g t b = SF { sfTF = tf0 }
  where
    (t', _) = randomExp g (1 / t)

    -- there can be no event at time of switching
    tf0 _ = (tfCont, NoEvent)
      where
        tfCont = afterExpAux 0 t'

    afterExpAux tCurr tEvt = SF' tf
      where
        tf dt _ 
            | tCurr' >= tEvt = (tf', Event b)
            | otherwise = (tf', NoEvent)
          where
            tCurr' = tCurr + dt
            tf' = afterExpAux tCurr' tEvt