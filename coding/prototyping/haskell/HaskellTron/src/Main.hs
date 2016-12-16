module Main where

-- NOTE: this implements a 2D-version of the famous Light-Cycle Game from Tron
--       the novelty is that time can be slowed down, halted and reversed so that one can escape lock-ins which
--       are cause by one owns bad decisions earlier or by enemies
--       This is a proof-of-concept of manipulating time in a game developed with Yampa/Dunai. The hypothesis is that
--          it should be very easy to do this as we can switch in/freeze SFs and also slow-down/stop/reverse time.

main :: IO ()
main = putStrLn "Hello Tron!"