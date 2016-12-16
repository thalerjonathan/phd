module Main where

import HaskellAgents
import HACModel
import Control.Monad.STM
import System.Random

import qualified HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

main :: IO ()
main = do
        let dt = 0.01
        let agentCount = 100
        let heroDistribution = 0.25
        let simStepsPerSecond = 30
        let rngSeed = 42
        let g = mkStdGen rngSeed
        (initAs, g') <- atomically $ createRandomHACAgents g agentCount heroDistribution
        GLO.simulateIO Front.display
            GLO.white
            simStepsPerSecond
            initAs
            modelToPicture
            (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: [HACAgent] -> IO GLO.Picture
modelToPicture as = return (Front.renderFrame observableAgentStates)
    where
        observableAgentStates = map hacAgentToObservableState as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
stepIteration :: Double -> GLO.ViewPort -> Float -> [HACAgent] -> IO [HACAgent]
stepIteration fixedDt viewport dtRendering as = atomically $ HaskellAgents.stepSimulation as fixedDt

hacAgentToObservableState :: HACAgent -> (Double, Double, Bool)
hacAgentToObservableState a = (x, y, h)
    where
        s = state a
        (x, y) = pos s
        h = hero s

{-
main :: IO ()
main = do
    let b = makeOffer bid
    let r = executeOffer ask b
    putStrLn (show r)
        where
            bid = Bid 42
            ask = Ask 41

data Offering a = Bid a | Ask a | Accept | Refuse deriving (Show)

data Conversation m = Msg m | Conv (m -> Conversation m)

{- NOTE: this part sends an offer enclosed in the lambda and will compare it to a passed in offer where a reply is then made
         TODO: introduce additional step> send o along with an additional reply and wait
-}
makeOffer :: Offering Int -> Conversation (Offering Int)
makeOffer o = Conv (\o' -> if compareOffer o o' then
                                Msg Accept
                                    else
                                        Msg Refuse )

{- NOTE: this part is the counterpart which allows to execute a given offering-conversation with a given offering
         problem: we loose track were we are like in makeOffer
         TODO: must always terminate and finally return Message

-}
executeOffer :: Offering Int -> Conversation (Offering Int) -> Offering Int
executeOffer o (Conv f) = executeOffer o (f o)
executeOffer o (Msg o') = o'

compareOffer :: Offering Int -> Offering Int -> Bool
compareOffer (Bid a) (Ask a') = a >= a'
compareOffer (Ask a) (Bid a') = a <= a'
compareOffer _ _ = False
-}