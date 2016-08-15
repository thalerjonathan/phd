{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 11:58:21 2003>
   License:    GPL
-}

{- DESCRIPTION

Example 2 - Do notation

Usage: Compile the code and execute the resulting program.
       It will print Dolly's maternal grandfather.
-}

import Control.Monad
import Data.Maybe

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- the Maybe type is already declared as an instance of the Monad class
-- in the standard prelude, so we don't actually need to define it here.
-- just remember that it looks something like this:
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just

-- we can use do-notation to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do m <- mother s
                           father m

maternalGrandmother :: Sheep -> Maybe Sheep
maternalGrandmother s = do m <- mother s
                           mother m

paternalGrandfather :: Sheep -> Maybe Sheep
paternalGrandfather s = do m <- father s
                           father m

paternalGrandmother :: Sheep -> Maybe Sheep
paternalGrandmother s = do m <- father s
                           mother m

maternalGrandfather_ :: Sheep -> Maybe Sheep
maternalGrandfather_ s = mother s >>= (\m -> father m)

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
				  mother gm

fathersMaternalGrandmother_ :: Sheep -> Maybe Sheep
fathersMaternalGrandmother_ s = father s >>= (\f -> mother f >>= (\gm -> mother gm) )

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
				  father gf

mothersPaternalGrandfather_ :: Sheep -> Maybe Sheep
mothersPaternalGrandfather_ s = mother s >>= (\m -> father m >>= (\gf -> father gf) )

parent_ :: Sheep -> Maybe Sheep
parent_ s = (mother s) `mplus` (father s)

parent :: Sheep -> [Sheep]
parent s = (maybeToList (mother s)) `mplus` (maybeToList (father s))

parent' :: (MonadPlus m) => Sheep -> m Sheep
parent' s = (toMonad (mother s)) `mplus` (toMonad (father s))

toMonad :: MonadPlus m => Maybe a -> m a
toMonad Nothing = mzero
toMonad (Just s) = return s

grandparent' :: MonadPlus m => Sheep -> m Sheep
grandparent' s = (toMonad (maternalGrandfather s)) `mplus`
                 (toMonad (maternalGrandmother s)) `mplus`
                 (toMonad (paternalGrandmother s)) `mplus`
                 (toMonad (paternalGrandfather s))

grandparent_ :: Sheep -> Maybe Sheep
grandparent_ s = maternalGrandfather s `mplus`
                 maternalGrandmother s `mplus`
                 paternalGrandmother s `mplus`
                 paternalGrandfather s

grandparent :: Sheep -> [Sheep]
grandparent s = let momMother = maternalGrandfather s
                    momDad = maternalGrandmother s
                    dadMother = paternalGrandmother s
                    dadDad = paternalGrandfather s
                in (maybeToList momMother) ++ (maybeToList momDad) ++ (maybeToList dadMother) ++ (maybeToList dadDad)

-- this builds our sheep family tree
breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing Nothing
		 uranus = Sheep "Uranus" Nothing Nothing
		 gaea   = Sheep "Gaea" Nothing Nothing
		 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
	         roger  = Sheep "Roger" (Just eve) (Just kronos)
	         molly  = Sheep "Molly" (Just holly) (Just roger)
	     in Sheep "Dolly" (Just molly) Nothing

-- print Dolly's maternal grandfather
main :: IO ()
main = let dolly = breedSheep
       in do print (maternalGrandfather dolly)
	
-- END OF FILE 
