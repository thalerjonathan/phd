{-# LANGUAGE Arrows #-}
module FrABS.Agent.Reactive (
  ) where

import FrABS.Agent.Agent

-- study arrowized programming (papers): how can dt disappear ? can we ommit arguments which are implicitly there?
-- develop arrowized EDSL for ABS: timeout transitions, rate transitions, sending messages after, repeatedly send message in interval, occasionally send message