module SuperSampling 
    (
      testSuperSampling
    ) where

import Data.Maybe

import FRP.Yampa.InternalCore



testSuperSampling :: IO ()
testSuperSampling = print "testSuperSampling"

-- NOTE: this is modified version of embed taken from Yampa
superSampling :: SF a b -> (a, [(DTime, Maybe a)]) -> ([b], SF a b)
superSampling sf0 (a0, dtas) = (b0 : bs, sfFrozen)
    where
        (sf, b0) = (sfTF sf0) a0
        (bs, sfUnfrozen) = loop a0 sf dtas

        lastDt = 0 --TODO: this is not correct yet
        sfFrozen = freeze sfUnfrozen 0

        loop _ sf [] = ([], sf)
        loop a_prev sf ((dt, ma) : dtas) = (b : bs', sf'')
            where
                a        = fromMaybe a_prev ma
                (sf', b) = (sfTF' sf) dt a
                (bs', sf'') = (a `seq` b `seq` loop a sf' dtas)

-- TODO: this was taken from Yampa, remove if Yampa exposes it
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}