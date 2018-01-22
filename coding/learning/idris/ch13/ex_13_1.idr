-- 1 is implemented in Door.idr

data GuessCmd : Type -> Nat -> Nat -> Type where
  Try : Integer -> GuessCmd Ordering (S n) n

  Pure : ty -> GuessCmd ty state state
  (>>=) : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3

threeGuesses : GuessCmd () 3 0
threeGuesses = do
  Try 10
  Try 20
  Try 15
  Pure ()

{-
noGuesses : GuessCmd () 0 0
noGuesses = do
  Try 42
  Pure ()
-}

namespace MatterCmd
  data Matter = Solid | Liquid | Gas

  data MatterCmd : Type -> Matter -> Matter -> Type where
    Freeze   : MatterCmd () Liquid Solid
    Condense : MatterCmd () Gas Liquid
    Boil     : MatterCmd () Liquid Gas
    Melt     : MatterCmd () Solid Liquid 
    
    Pure     : t -> MatterCmd t s s
    (>>=)    : MatterCmd a s1 s2 -> (a -> MatterCmd b s2 s3) -> MatterCmd b s1 s3

  iceStream : MatterCmd () Solid Gas
  iceStream = do
    Melt
    Boil

  steamIce : MatterCmd () Gas Solid
  steamIce = do
    Condense
    Freeze

  {-
  overMelt : MatterCmd () Solid Gas
  overMelt = do
    Melt
    Melt
    -}