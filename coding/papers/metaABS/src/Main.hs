module Main where

import SIRS.RunSIRS
import Segregation.SegregationRun
import MetaABS.MetaABSRun
import SugarScape.SugarScapeRun
import Conversation.ConversationRun

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.Pure.Animate

main :: IO ()
main = runConversationStepsAndPrint
    -- runSugarScapeWithRendering
    -- runMetaABSStepsAndPrint
    -- runSIRSWithRendering
    -- runSegWithRendering
    -- runSegStepsAndRender
    -- test
    -- testGloss

test :: IO ()
test = do
        let x = -10
        let y = max 0 x
        putStrLn ("y = " ++ (show y))


testGloss :: IO ()
testGloss = do
                GLO.display (GLO.InWindow "Test Gloss" (800, 800) (0, 0))
                            GLO.white
                            (GLO.Pictures $ [renderScaledCircle, renderReferenceCircle])

renderReferenceCircle :: Picture
renderReferenceCircle = GLO.color color $ GLO.translate x y $ GLO.Circle radius
    where
        x = 0
        y = 0
        radius = 200
        color = GLO.makeColor (realToFrac 0.8) (realToFrac 0.0) (realToFrac 0.0) 1.0

renderScaledCircle :: Picture
renderScaledCircle = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 400
    where
        x = 0
        y = 0

        color = GLO.makeColor (realToFrac 0.9) (realToFrac 0.9) (realToFrac 0.0) 1.0