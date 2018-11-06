module SugarScape.Agent.Utils 
  ( continueWithAfter
  ) where

import Data.Maybe

import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

-- this is a simplified version of switch WITH DIFFERENT SEMANTICS:
-- at the time of the switching the new MSF is NOT activated but only in the next step!

continueWithAfter :: Monad m => MSF m a (b, Maybe (MSF m a b)) -> MSF m a b
continueWithAfter msf = MSF $ \a -> do
  ((b, msfCont), msf') <- unMSF msf a
  let msfNext = fromMaybe (continueWithAfter msf') msfCont
  return (b, msfNext)