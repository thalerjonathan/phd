-- ITU FRP 2010: Exercice 1: DIY FRP
-- Author: Henrik Nilsson
-- Date: 2010-06-20

module DIYFRP where

import Prelude hiding (until)

infix  4 ==*, /=*, <*, <=*, >=*, >*
infixr 3 &&*
infixr 2 ||*
infixr 1 `until`, ==>, -=>
infixl 0 $*


------------------------------------------------------------------------------
-- Basic types
------------------------------------------------------------------------------

type Time = Double


-- Note: It is assumed that the lists in the definitions of B and E actually
-- are *streams*: "infinite" lists. It would have been more precise to define
-- a stream type:
--
--     data S a = SCons a (S a)
--
-- But then we would also have to define a range of functions on streams,
-- in many cases duplicating what is already available, and we would not
-- be able to use any of the list-specific syntax, a syntax which is quite
-- convenient. So, for this simple example, we will stick with representing
-- streams by infinite lists, and allow all definitions to assume the lists
-- are indeed infinite.


-- Representation of a behavior. Note: a conceptually continuous-time signal
-- *generator*.

newtype B a = B ([Time] -> [a])


-- Representation of an event. Note: a discrete-time signal *generator*.
-- Also note that an event may occur many times (just like a recurring
-- event in real-life, like an annual festival).

newtype E a = E ([Time] -> [Maybe a])


------------------------------------------------------------------------------
-- Reactimations and pretty priting utlity
------------------------------------------------------------------------------

-- Function to animate (run) a behavior from time 0. In a real implementation,
-- this would also arrange to feed input into the system. Hence "reactimate".
-- end ........	End time, >= 0
-- step .......	Step size, > 0
-- b ..........	The beavior to animate
-- Returns finite list of resulting time-value pairs up to the specified
-- end time.

reactimateB :: Time -> Time -> B a -> [(Time, a)]
reactimateB end step b@(B f) | end >= 0 && step > 0 = 
    takeWhile (\(t, _) -> t <= end) (zip ts (f ts))
    where
        ts = [0.0, step ..]


-- Function to animate (run) an event from time 0. In a real implementation,
-- this would also arrange to feed input into the system. Hence "reactimate".
-- end ........	End time, >= 0
-- step .......	Step size, > 0
-- e ..........	The event to animate
-- Returns time-stamped event occurrences up to the specified end time.

reactimateE :: Time -> Time -> E a -> [(Time, a)]
reactimateE end step e@(E f) | end >= 0 && step > 0 = 
    [ (t, a) | (t, Just a) <-takeWhile (\(t, _) -> t <= end) (zip ts (f ts)) ]
    where
        ts = [0.0, step ..]


-- Utility to show lists of time-value pairs in a readable way.
-- d ..........	Number of decimal places to use for the time part.
-- tas ........	(Finite) list of time-value pairs.

ppTrace :: Show a => Int -> [(Time, a)] -> IO ()
ppTrace d tas = mapM_ ppTA tas
    where
        ppTA :: Show a => (Time, a) -> IO ()
        ppTA (t, a) = do
            putStr (fmtTime t)
            putStr ": "
            putStrLn (show a)

        fmtTime :: Time -> String
        fmtTime t = take (14 - length t') (repeat ' ') ++ t' 
            where
                s  = 10 ^ d
                t' = show (fromIntegral (round (t * s)) / s)


------------------------------------------------------------------------------
-- Basic behaviors
------------------------------------------------------------------------------

time :: B Time
time = undefined


($*) :: B (a -> b) -> B a -> B b
(B ff) $* (B fa) = B $ \ts -> zipWith ($) (ff ts) (fa ts)


lift0 :: a -> B a
lift0 a = undefined


-- One could also have defined lift0:
-- lift0 a = B (const (repeat a))
-- This would be lazier as the input is completely ignored and an
-- infinite result sream is generated no matter what.


lift1 :: (a -> b) -> (B a -> B b)
lift1 f ba = undefined


lift2 :: (a -> b -> c) -> (B a -> B b -> B c) 
lift2 f ba bb = undefined


------------------------------------------------------------------------------
-- Standard behavior and event instances and liftings (for convenience)
------------------------------------------------------------------------------

-- Note: Needed to allow B to be an instance of Num
instance Show (B a) where
    show _ = "<Behavior>"


instance Show (E a) where
    show _ = "<Event>"


-- Note: Needed to allow B to be an instance of Num
instance Eq (B a) where
    _ == _ = error "Dummy Eq Instance: use (==*)"
    _ /= _ = error "Dummy Eq Instance: use (/=*)"


(==*) :: Eq a => B a -> B a -> B Bool
(==*) = undefined

(/=*) :: Eq a => B a -> B a -> B Bool
(/=*) = undefined


instance Ord a => Ord (B a) where
    compare x y = error "Dummy Ord instance: compareB"
    _ < _       = error "Dummy Ord instance: use (<*)"
    _ <= _      = error "Dummy Ord instance: use (<=*)"
    _ >= _      = error "Dummy Ord instance: use (>=*)"
    _ > _       = error "Dummy Ord instance: use (>*)"
    min x y     = error "Dummy Ord instance: use minB"
    max x y     = error "Dummy Ord instance: use maxB"


compareB :: Ord a => B a -> B a -> B Ordering
compareB = undefined

(<*) :: Ord a => B a -> B a -> B Bool
(<*) = undefined

(<=*) :: Ord a => B a -> B a -> B Bool
(<=*) = undefined

(>=*) :: Ord a => B a -> B a -> B Bool
(>=*) = undefined

(>*) :: Ord a => B a -> B a -> B Bool
(>*) = undefined

minB :: Ord a => B a -> B a -> B a
minB = undefined

maxB :: Ord a => B a -> B a -> B a
maxB = undefined


instance Num a => Num (B a) where
    (+)         = undefined
    (*)         = undefined
    (-)         = undefined
    abs         = undefined
    signum      = undefined
    fromInteger = undefined


instance Fractional a => Fractional (B a) where
    (/)          = undefined
    recip        = undefined
    fromRational = undefined


instance (Ord a, Floating a) => Floating (B a) where
    pi    = undefined
    exp   = undefined
    log   = undefined
    sqrt  = undefined
    sin   = undefined
    cos   = undefined
    sinh  = undefined
    cosh  = undefined
    asin  = undefined
    acos  = undefined
    atan  = undefined
    asinh = undefined
    acosh = undefined
    atanh = undefined


notB ::B Bool -> B Bool
notB = undefined

(&&*) :: B Bool -> B Bool -> B Bool
(&&*) = undefined

(||*) :: B Bool -> B Bool -> B Bool
(||*) = undefined


------------------------------------------------------------------------------
-- Basic events
------------------------------------------------------------------------------

never :: E a
never = undefined

now :: E ()
now = undefined


------------------------------------------------------------------------------
-- Time events
------------------------------------------------------------------------------

-- A single event as soon as possible after the indicated amount of time
-- has *passed*.

after :: Time -> E ()
after td | td >= 0 = undefined


-- Suggested test:
-- reactimateE 10 0.1 (after 0.55 ())  


-- Repeated events with an interval as close as possible to the
-- indicated period. (What to do if very sparsely sampled?)

repeatedly :: Time -> E ()
repeatedly tp | tp > 0 = undefined

-- Suggested tests:
-- reactimateE 10 0.1 (repeatedly 0.55 ())
-- reactimateE 10 1 (repeatedly 0.01 ())


------------------------------------------------------------------------------
-- Predicate events
------------------------------------------------------------------------------

-- Event whenever b changes from False to True

edge :: B Bool -> E ()
edge (B fb) = undefined


-- Suggested test: 
-- reactimateE 10 0.1 (edge (time >* 5.0 &&* time <* 6.0 ||* time >* 8.01))


------------------------------------------------------------------------------
-- Event mapping
------------------------------------------------------------------------------

(==>) :: E a -> (a -> b) -> E b
E fe ==> f = undefined


(-=>) :: E a -> b -> E b
e -=> b = e ==> (const b)


------------------------------------------------------------------------------
-- Event counting
------------------------------------------------------------------------------

-- Event counter: generate an event carrying the total number of event
-- occurrences thus for the argument event.

count :: E a -> E Integer
count (E fe) = undefined


------------------------------------------------------------------------------
-- Mediation event to behavior
------------------------------------------------------------------------------

-- Conversion of event to piecwise constant behavior (or step behavior)

hold :: a -> E a -> B a
hold a0 (E fe) = undefined


------------------------------------------------------------------------------
-- Switch
------------------------------------------------------------------------------

until :: B a -> E (B a) -> B a
(B fb) `until` (E fe) = undefined


test_until :: B Integer
test_until =
    let
        c = hold 0 (count (repeatedly 0.5))
    in
        c `until` after 5 -=> c * 2


------------------------------------------------------------------------------
-- Integral
------------------------------------------------------------------------------

integral :: B Double -> B Double
integral (B b) = undefined


-- Test cases:
-- integral 1.0
-- integral time
-- let et = 1 + integral et in et
-- let x = 1 + integral (x * x) in x

-- The "e^t" example does *not* work with the first version. Which
-- is as it should be.
-- Does work with the second version. But slowly slows down!
-- Try e.g. "reactimateB 100 0.001 (let et = 1 + integral et in et))".

