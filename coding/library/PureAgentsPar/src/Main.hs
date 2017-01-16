module Main where

import HeroesAndCowards.RunHAC

import SIRS.RunSIRS

import WildFire.RunWFDynamic
import WildFire.RunWFStatic

import SpacialGame.RunSG

main :: IO ()
main = runSGWithRendering