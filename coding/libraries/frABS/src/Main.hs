module Main where

import SIRS.RunSIRS
import Segregation.SegregationRun
import MetaABS.MetaABSRun
import SugarScape.SugarScapeRun
import Conversation.ConversationRun
import MessageSampling.MessageSamplingRun

main :: IO ()
main = runSugarScapeWithRendering

    -- runMessageSamplingAndPrint
    -- runConversationStepsAndPrint
    -- runSugarScapeWithRendering
    -- runMetaABSStepsAndPrint
    -- runSIRSWithRendering
    -- runSegWithRendering
    -- runSegStepsAndRender
    -- test
    -- testGloss

{-
testCulturalCrossover :: IO ()
testCulturalCrossover = 
	do
		let g = mkStdGen 42
		let tag1 = [True, False, True, False]
		let tag2 = [True, True, False, False]
		let (tag', g') = runRand (cc tag1 tag2) g
		putStrLn (show tag')

cc :: (RandomGen g) => [Bool] -> [Bool] -> Rand g [Bool] 
cc ts1 ts2 = 
	do
		randTags <- replicateM zipLen (getRandomR (True, False))
		return map (\(t1, t2, randT) -> if t1 == t2 then t1 else randT) (zip3 ts1 ts2 randTags)

    where
        tagZip = zip ts1 ts2
        zipLen = length tagZip
        tagsIdentical = map (\(t1, t2) -> t1 == t2) tagZip 	-- is true where the values are identical (also in case of both are False)
        tagsAnd = map (\(t1, t2) -> t1 && t2) tagZip		-- is True when both values are True and False in other cases (also in case both are False)
-}