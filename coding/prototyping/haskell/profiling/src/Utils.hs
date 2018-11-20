module Utils where

import Data.Maybe

import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

continueWithAfter :: Monad m => MSF m a (b, Maybe (MSF m a b)) -> MSF m a b
continueWithAfter msf = MSF $ \a -> do
  ((b, msfCont), msf') <- unMSF msf a
  let msfNext = fromMaybe (continueWithAfter msf') msfCont
  return (b, msfNext)