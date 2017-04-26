{-# OPTIONS --type-in-type
   #-}

module SigVecs where

open import NeilPrelude
open import RealTime
open import TimeDeltaList

import SVDesc
open SVDesc public

------------------------------------------------------

-- The change prefixes are inclusive of the time argument
-- (i.e. any changes at that point in time will appear in the list)

ChangeList = TimeDeltaList

ChangePrefix : Set → Set
ChangePrefix A = Time → ChangeList A

SigVec : SVDesc → Set
SigVec (C A)      =  Time → A
SigVec (E A)      =  Maybe A × ChangePrefix A
SigVec (S A)      =  A × ChangePrefix A
SigVec (as , bs)  =  SigVec as × SigVec bs

SF : SVDesc → SVDesc → Set
SF as bs = SigVec as → SigVec bs

------------------------------------------------------

SVRep : SVDesc → Set
SVRep (C A) = A
SVRep (E A) = Maybe A × ChangeList A
SVRep (S A) = A × ChangeList A
SVRep (as , bs) = SVRep as × SVRep bs

rep : {as : SVDesc} → SigVec as → Time → SVRep as
rep {C _} s t             = s t
rep {E _} (ma , cp) t     = (ma , cp t)
rep {S _} (a , cp) t      = (a , cp t)
rep {as , bs} (sa , sb) t = (rep {as} sa t , rep {bs} sb t)

------------------------------------------------------
