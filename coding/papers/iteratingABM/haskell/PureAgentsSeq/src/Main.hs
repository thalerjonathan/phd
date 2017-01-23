module Main where

import WildFire.RunWFDynamic
import WildFire.RunWFStatic

import SIRS.RunSIRS

import HeroesAndCowards.RunHAC

import SpacialGameMsg.RunSGMsg
import SpacialGameEnv.RunSGEnv

main :: IO ()
main = runSGMsgWithRendering