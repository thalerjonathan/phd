{-# LANGUAGE Arrows #-}

module Main where

import SIRS.RunSIRS

import FRP.Yampa
import FRP.Yampa.Switches

{-
main :: IO ()
main = runSIRSWithRendering
-}

main :: IO ()
main = do
        let sts = take 10 $ samplingTimes 0.0 1.0
        let os = embed kSwitchTest (42, sts)
        mapM putStrLn os
        return ()

samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt

type TestInput = Int
type TestOutput = String

simpleSF :: SF TestInput TestOutput
simpleSF = proc i ->
                do
                    t <- time -< 0
                    let o = "i = " ++ (show i) ++ " at t = " ++ (show t)
                    returnA -< (show o)

------------------------------------------------------------------------------------------------------------------------
-- d/SWITCH
------------------------------------------------------------------------------------------------------------------------
-- NOTE: a dSwitch does not prevent one from recurring when using initial SF as function to switch into
switchTest :: SF TestInput TestOutput
switchTest = dSwitch
                switchTrigger
                switchContinuation

switchTrigger:: SF TestInput (TestOutput, Event String)
switchTrigger = proc i ->
                        do
                            t <- time -< 0
                            let o = show t
                            returnA -< (o, Event "Event Occured")

-- NOTE: a dSwitch does not prevent one from recurring when using initial SF as function to switch into
switchContinuation :: String -> SF TestInput TestOutput
switchContinuation evtData = proc i ->
                                  do
                                    let o = evtData ++ " with input: " ++ (show i)
                                    returnA -< o
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- d/rSwitch
------------------------------------------------------------------------------------------------------------------------
rSwitchTest :: SF TestInput TestOutput
rSwitchTest = proc i ->
                do
                    let iTup = (i+1, NoEvent) -- Event simpleSF)
                    drSwitch simpleSF -< iTup
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- d/kSwitch
------------------------------------------------------------------------------------------------------------------------
kSwitchTest :: SF TestInput TestOutput
kSwitchTest = proc i ->
                do
                    let i' = i + 1
                    kSwitch
                        simpleSF
                        kSwitchTrigger
                        kSwitchContinuation -< i'

kSwitchTrigger :: SF (TestInput, TestOutput) (Event String)
kSwitchTrigger = proc (i, o) ->
                        do
                            let e = Event ("Event Switch with input = " ++ (show i) ++ " and output " ++ o)
                            returnA -< e

kSwitchContinuation :: SF TestInput TestOutput -> String -> SF TestInput TestOutput
kSwitchContinuation oldSf evtData = oldSf -- simpleSF
