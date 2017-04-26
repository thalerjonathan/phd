{-# OPTIONS --type-in-type  #-}

open import NeilPrelude
open import SigVecs
open import RealTime
open import Properties
open import NaryFRP
open import CausalityProps
open import PrimProps

module Loop where

----------------------------------------------------------------------------------------------------------

bad : {as bs : SVDesc} → SF as bs
bad {as} {bs} = loop {as} {bs} {bs} (sfSnd {as} {bs}) (identity {bs})

----------------------------------------------------------------------------------------------------------

loop₀ : {bs : SVDesc} → SF bs bs → SigVec bs
loop₀ sf = fix sf

loop₁ : {bs cs : SVDesc} → SF cs bs → SF bs cs → SigVec bs
loop₁ {bs} sff sfb = loop₀ {bs} (sfb ⋙ sff)

loop₂ : {as bs cs : SVDesc} → SF (as , cs) bs → SF bs cs → SF as bs
loop₂ {as} {bs} {cs} sff sfb sa = loop₁ {bs} {cs} (λ sc → sff (sa , sc)) sfb

----------------------------------------------------------------------------------------------------------

arrowLoop : {as bs cs : SVDesc} → SF (as , cs) (bs , cs) → SF as bs
arrowLoop {as} {bs} {cs} sf = loop {as} {bs , cs} {cs} sf (sfSnd {bs} {cs}) ⋙ sfFst {bs} {cs}

symLoop' : {as bs cs ds : SVDesc} → SF (as , cs) (bs , ds) → SF ds cs → SF as bs
symLoop' {as} {bs} {cs} {ds} sff sfb = loop {as} {bs , ds} {cs} sff (sfSnd {bs} {ds} ⋙ sfb) ⋙ sfFst {bs} {ds}

----------------------------------------------------------------------------------------------------------

postulate verboseLoop : {as bs cs : SVDesc} → (sff : SF (as , cs) bs) → (sfb : SF bs cs) → StrictlyDec {as , cs} {bs} sff ⊎ StrictlyDec {bs} {cs} sfb ⊎ (Decoupled {as , cs} {bs} sff × DiscreteSV bs) ⊎ (Decoupled {bs} {cs} sfb × DiscreteSV bs) → SF as bs

----------------------------------------------------------------------------------------------------------

CSF : SVDesc → SVDesc → Set
CSF as bs = Σ (SF as bs) (Causal {as} {bs})

DSF : SVDesc → SVDesc → Set
DSF as bs = Σ (SF as bs) (Decoupled {as} {bs})

IsDec : {as bs : SVDesc} → CSF as bs → Set
IsDec {as} {bs} sf = Decoupled {as} {bs} (fst sf)

----------------------------------------------------------------------------------------------------------

lem-dec-seq : {bs cs : SVDesc} → (sfb : SF bs cs) → (sff : SF cs bs) → Causal {bs} {cs} sfb → Causal {cs} {bs} sff → Decoupled {bs} {cs} sfb ⊎ Decoupled {cs} {bs} sff → Decoupled {bs} {bs} (sfb ⋙ sff)
lem-dec-seq {bs} {cs} sfb sff caub cauf (inl decb) = Decoupled1Causal2→DecoupledSeq {bs} {cs} {bs} sfb sff decb cauf
lem-dec-seq {bs} {cs} sfb sff caub cauf (inr decf) = Causal1Decoupled2→DecoupledSeq {bs} {cs} {bs} sfb sff caub decf

lem-cau-inp : {as bs cs : SVDesc} → (sa : SigVec as) → (sff : SF (as , cs) bs) → Causal {as , cs} {bs} sff → Causal {cs} {bs} (λ sc → sff (sa , sc))
lem-cau-inp sa sff cau sc₁ sc₂ t = cau (sa , sc₁) (sa , sc₂) t ∘ ×-congL ∥ result2' ×-congL

lem-dec-inp : {as bs cs : SVDesc} → (sa : SigVec as) → (sff : SF (as , cs) bs) → Decoupled {as , cs} {bs} sff → Decoupled {cs} {bs} (λ sc → sff (sa , sc))
lem-dec-inp sa sff dec sc₁ sc₂ t = dec (sa , sc₁) (sa , sc₂) t ∘ result2' ×-congL

lem-dec-inpR : {as bs cs : SVDesc} → (sa : SigVec as) → (sfb : SF bs cs) → (sff : SF (as , cs) bs) → Decoupled {bs} {cs} sfb ⊎ Decoupled {as , cs} {bs} sff → Decoupled {bs} {cs} sfb ⊎ Decoupled {cs} {bs} (λ sc → sff (sa , sc))
lem-dec-inpR {as} {bs} {cs} sa sfb sff = right (lem-dec-inp {as} {bs} {cs} sa sff)

lem-decR : {bs cs ds : SVDesc} → (sfb : DSF ds cs) → Decoupled {bs , ds} {cs} (sfSnd {bs} {ds} ⋙ fst sfb)
lem-decR {bs} {cs} {ds} (sfb , dec) = Causal1Decoupled2→DecoupledSeq {bs , ds} {ds} {cs} (sfSnd {bs} {ds}) sfb (Causal-sfSnd {bs} {ds}) dec

lem-cauR : {bs cs ds : SVDesc} → (sfb : DSF ds cs) → Causal {bs , ds} {cs} (sfSnd {bs} {ds} ⋙ fst sfb)
lem-cauR {bs} {cs} {ds} sfb = Decoupled→Causal {bs , ds} {cs} (sfSnd {bs} {ds} ⋙ fst sfb) (lem-decR {bs} {cs} {ds} sfb)

----------------------------------------------------------------------------------------------------------

postulate sffix : {as : SVDesc} → DSF as as → SigVec as

dloop₀ : {as : SVDesc} → DSF as as → SigVec as
dloop₀ {as} sf = sffix {as} sf

dloop₁ : {bs cs  : SVDesc} → (sff : CSF cs bs) → (sfb : CSF bs cs) → IsDec {bs} {cs} sfb ⊎ IsDec {cs} {bs} sff → SigVec bs
dloop₁ {bs} {cs} (sff , cauf) (sfb , caub) dec = dloop₀ {bs} ((sfb ⋙ sff) , lem-dec-seq {bs} {cs} sfb sff caub cauf dec)

dloop₂ : {as bs cs : SVDesc} → (sff : CSF (as , cs) bs) → (sfb : CSF bs cs) → IsDec {bs} {cs} sfb ⊎ IsDec {as , cs} {bs} sff → SF as bs
dloop₂ {as} {bs} {cs} (sff , cauf) (sfb , caub) dec sa = dloop₁ {bs} {cs} ((λ sc → sff (sa , sc)) , lem-cau-inp {as} {bs} {cs} sa sff cauf) (sfb , caub) (lem-dec-inpR {as} {bs} {cs} sa sfb sff dec)

----------------------------------------------------------------------------------------------------------

dloop : {as bs cs : SVDesc} → (sff : CSF (as , cs) bs) → (sfb : CSF bs cs) → IsDec {bs} {cs} sfb ⊎ IsDec {as , cs} {bs} sff → SF as bs
dloop {as} {bs} {cs} = dloop₂ {as} {bs} {cs}

darrowLoop : {as bs cs : SVDesc} → DSF (as , cs) (bs , cs) → SF as bs
darrowLoop {as} {bs} {cs} (sf , dec) = dloop {as} {bs , cs} {cs}
                                             (sf , Decoupled→Causal {as , cs} {bs , cs} sf dec)
                                             (sfSnd {bs} {cs} , Causal-sfSnd {bs} {cs}) (inr dec)
                                       ⋙ sfFst {bs} {cs}

dsymLoop : {as bs cs ds : SVDesc} → CSF (as , cs) (bs , ds) → DSF ds cs → SF as bs
dsymLoop {as} {bs} {cs} {ds} sff sfb = dloop {as} {bs , ds} {cs}
                                             sff
                                             (sfSnd {bs} {ds} ⋙ (fst sfb) , lem-cauR {bs} {cs} {ds} sfb)
                                             (inl (lem-decR {bs} {cs} {ds} sfb))
                                       ⋙ sfFst {bs} {ds}


----------------------------------------------------------------------------------------------------------

SDSF : SVDesc → SVDesc → Set
SDSF as bs = Σ (SF as bs) (StrictlyDec {as} {bs})

IsSDec : {as bs : SVDesc} → CSF as bs → Set
IsSDec {as} {bs} sf = StrictlyDec {as} {bs} (fst sf)

----------------------------------------------------------------------------------------------------------

lem-sdec-seq : {bs cs : SVDesc} → (sfb : SF bs cs) → (sff : SF cs bs) → Causal {bs} {cs} sfb → Causal {cs} {bs} sff → StrictlyDec {bs} {cs} sfb ⊎ StrictlyDec {cs} {bs} sff → StrictlyDec {bs} {bs} (sfb ⋙ sff)
lem-sdec-seq {bs} {cs} sfb sff caub cauf (inl decb) = StrictlyDec1Causal2→StrictlyDecSeq {bs} {cs} {bs} sfb sff decb cauf
lem-sdec-seq {bs} {cs} sfb sff caub cauf (inr decf) = Causal1StrictlyDec2→StrictlyDecSeq {bs} {cs} {bs} sfb sff caub decf

lem-sdec-inp : {as bs cs : SVDesc} → (sa : SigVec as) → (sff : SF (as , cs) bs) → StrictlyDec {as , cs} {bs} sff → StrictlyDec {cs} {bs} (λ sc → sff (sa , sc))
lem-sdec-inp {as} {bs} {cs} sa sff (δ , dec) = δ , lem-sdec-inp-aux
  where
        lem-sdec-inp-aux : StrictlyDecAux {cs} {bs} (λ sc → sff (sa , sc)) δ
        lem-sdec-inp-aux sc₁ sc₂ t eqHeq with dec (sa , sc₁) (sa , sc₂) t
        ... | p with compareGeq t (δ >0)
        ...     | less q = p _
        ...     | geq  q = p (×-congL (fst eqHeq) , result2' ×-congL (snd eqHeq))

lem-sdec-inpR : {as bs cs : SVDesc} → (sa : SigVec as) → (sfb : SF bs cs) → (sff : SF (as , cs) bs) → StrictlyDec {bs} {cs} sfb ⊎ StrictlyDec {as , cs} {bs} sff → StrictlyDec {bs} {cs} sfb ⊎ StrictlyDec {cs} {bs} (λ sc → sff (sa , sc))
lem-sdec-inpR {as} {bs} {cs} sa sfb sff = right (lem-sdec-inp {as} {bs} {cs} sa sff)

lem-sdecR : {bs cs ds : SVDesc} → (sfb : SDSF ds cs) → StrictlyDec {bs , ds} {cs} (sfSnd {bs} {ds} ⋙ fst sfb)
lem-sdecR {bs} {cs} {ds} (sfb , dec) = Causal1StrictlyDec2→StrictlyDecSeq {bs , ds} {ds} {cs} (sfSnd {bs} {ds}) sfb (Causal-sfSnd {bs} {ds}) dec

lem-SDcauR : {bs cs ds : SVDesc} → (sfb : SDSF ds cs) → Causal {bs , ds} {cs} (sfSnd {bs} {ds} ⋙ fst sfb)
lem-SDcauR {bs} {cs} {ds} sfb = StrictlyDec→Causal {bs , ds} {cs} (sfSnd {bs} {ds} ⋙ fst sfb) (lem-sdecR {bs} {cs} {ds} sfb)

----------------------------------------------------------------------------------------------------------

postulate sffixSD : {as : SVDesc} → SDSF as as → SigVec as

sdloop₀ : {as : SVDesc} → SDSF as as → SigVec as
sdloop₀ {as} sf = sffixSD {as} sf

sdloop₁ : {bs cs  : SVDesc} → (sff : CSF cs bs) → (sfb : CSF bs cs) → IsSDec {bs} {cs} sfb ⊎ IsSDec {cs} {bs} sff → SigVec bs
sdloop₁ {bs} {cs} (sff , cauf) (sfb , caub) dec = sdloop₀ {bs} ((sfb ⋙ sff) , lem-sdec-seq {bs} {cs} sfb sff caub cauf dec)

sdloop₂ : {as bs cs : SVDesc} → (sff : CSF (as , cs) bs) → (sfb : CSF bs cs) → IsSDec {bs} {cs} sfb ⊎ IsSDec {as , cs} {bs} sff → SF as bs
sdloop₂ {as} {bs} {cs} (sff , cauf) (sfb , caub) dec sa = sdloop₁ {bs} {cs} ((λ sc → sff (sa , sc)) , lem-cau-inp {as} {bs} {cs} sa sff cauf) (sfb , caub) (lem-sdec-inpR {as} {bs} {cs} sa sfb sff dec)

----------------------------------------------------------------------------------------------------------

sdloop : {as bs cs : SVDesc} → (sff : CSF (as , cs) bs) → (sfb : CSF bs cs) → IsSDec {bs} {cs} sfb ⊎ IsSDec {as , cs} {bs} sff → SF as bs
sdloop {as} {bs} {cs} = sdloop₂ {as} {bs} {cs}

sdarrowLoop : {as bs cs : SVDesc} → SDSF (as , cs) (bs , cs) → SF as bs
sdarrowLoop {as} {bs} {cs} (sf , dec) = sdloop {as} {bs , cs} {cs}
                                               (sf , StrictlyDec→Causal {as , cs} {bs , cs} sf dec)
                                               (sfSnd {bs} {cs} , Causal-sfSnd {bs} {cs}) (inr dec)
                                        ⋙ sfFst {bs} {cs}

sdsymLoop : {as bs cs ds : SVDesc} → CSF (as , cs) (bs , ds) → SDSF ds cs → SF as bs
sdsymLoop {as} {bs} {cs} {ds} sff sfb = sdloop {as} {bs , ds} {cs}
                                               sff
                                               (sfSnd {bs} {ds} ⋙ (fst sfb) , lem-SDcauR {bs} {cs} {ds} sfb)
                                               (inl (lem-sdecR {bs} {cs} {ds} sfb))
                                        ⋙ sfFst {bs} {ds}

----------------------------------------------------------------------------------------------------------
