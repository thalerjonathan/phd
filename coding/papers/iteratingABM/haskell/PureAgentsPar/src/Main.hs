module Main where

import HeroesAndCowards.RunHAC

import SIRS.RunSIRS

import WildFire.RunWFStatic

import SpacialGameMsg.RunSGMsg
import SpacialGameEnv.RunSGEnv

main :: IO ()
main = runSGEnvWithRendering

-- runSGEnvWithRendering