module Main where
import Graphics.UI.WX

radius, maxX, maxY :: Int
radius = 10
maxX = 300
maxY = 300

maxH :: Int
maxH = maxY - radius

main :: IO ()
main = start hello

hello :: IO ()
hello = do f <- frame [text := "Hello!"]
           quit <- button f [text := "Quit", on command := close f]
           set f [layout := widget quit] 
