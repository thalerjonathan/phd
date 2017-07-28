{-# LANGUAGE Arrows #-}
module Zombies.Agent (
	zombieBehaviour,
	humanBehaviour
  ) where

import           Zombies.Model

import           FRP.FrABS

import           FRP.Yampa

human :: ZombiesAgentBehaviour
human = transitionOnMessage
                Infect
                humanBehaviour
                zombie

humanBehaviour :: ZombiesAgentBehaviour
humanBehaviour = humanBehaviour

zombie :: ZombiesAgentBehaviour
zombie = humanBehaviour
------------------------------------------------------------------------------------------------------------------------