module Main where

import HeroesAndCowards.RunHAC

import SIRS.RunSIRS

import SpacialGameMsg.RunSGMsg
import SpacialGameEnv.RunSGEnv

import WildFire.RunWFStatic
import WildFire.RunWFDynamic

main :: IO ()
main = runSGMsgWithRendering
        -- runHAC

        -- runWFDynamicRendering
        -- runWFStaticRendering

        -- runSGMsgWithRendering
        -- runSGEnvWithRendering

        -- runSIRSRendering