{-# OPTIONS --type-in-type
    #-}

open import NeilPrelude
open import Recursion
open import List
open import Nat

module ListProps where


-- Properties of Lists on a Specific Set --

module ListPropsAux {A : Set} where

  ∷-inj : Injective2 {A} _∷_
  ∷-inj refl = refl , refl

  ∷-cong : Congruent2 {A} _∷_
  ∷-cong = cong2 _∷_

  ∷-congL : Congruent2L {A} _∷_
  ∷-congL = cong2L _∷_

  ∷-congR : Congruent2R {A} _∷_
  ∷-congR = cong2R _∷_

  ++assoc : {bs cs : List A} → (as : List A) → (as ++ bs) ++ cs ≡ as ++ bs ++ cs
  ++assoc = listrec refl ∷-congL

  ++[]' : (as : List A) → as ++ [] ≡ as
  ++[]' = listrec refl ∷-congL

  ++[] : {as : List A} → as ++ [] ≡ as
  ++[] {as} = ++[]' as

  import Monoid
  open Monoid _++_ (λ {as} → ++assoc as) [] refl ++[] public

  import SemiGroup
  open SemiGroup _++_ (λ {as} → ++assoc as) public

  listIdem : {f : A → A} → ({a : A} → f (f a) ≡ f a) → (as : List A) → map f (map f as) ≡ map f as
  listIdem eIdem = listrec refl (∷-cong eIdem)

  ++length : {bs : List A} → (as : List A) → length (as ++ bs) ≡ length as + length bs
  ++length = listrec refl (cong S)

  ++lengthC : {bs : List A} → (as : List A) → length as + length bs ≡ length (as ++ bs)
  ++lengthC = sym ∘' ++length

  +++length : {cs : List A} → (as bs : List A) → length (as ++ bs ++ cs) ≡ length as + length bs + length cs
  +++length as bs =  trans2 (cong length (assocR {as})) (++length (as ++ bs)) (cong2 _+_ (++length as) refl)

  +++lengthC : {cs : List A} → (as bs : List A) → length as + length bs + length cs ≡ length (as ++ bs ++ cs) 
  +++lengthC as bs = sym (+++length as bs)


  lengthZero : {as : List A} → length as ≡ O → as ≡ []
  lengthZero {[]} _ = refl
  lengthZero {_ ∷ _} ()

  mapId' : (as : List A) → map id as ≡ as
  mapId' = listrec refl ∷-congL

  mapId : {as : List A} → map id as ≡ as
  mapId {as} = mapId' as

  -- might need something else (other than just listrec) for ++rev, not sure

  ++rev : {as : List A} → (bs : List A) → rev as bs ≡ reverse bs ++ as
  ++rev []       = refl
  ++rev (b ∷ bs) = trans2 (++rev bs) (assocR {reverse bs}) (cong2R _++_ (sym (++rev bs)))

  slowReverse : List A → List A
  slowReverse [] = []
  slowReverse (a ∷ as) = slowReverse as ++ [ a ]

  reversesEq : (as : List A) → reverse as ≡ slowReverse as
  reversesEq = listrec' refl (λ _ as hyp → trans (++rev as) (cong2R _++_ hyp))

  ++reverseAux : {a : A} → {bs : List A} → (as : List A) → reverse (a ∷ as ++ bs) ≡ (reverse (as ++ bs)) ++ [ a ]
  ++reverseAux {a} {bs} as = trans (reversesEq (a ∷ as ++ bs)) (cong2R _++_ (sym (reversesEq (as ++ bs))))

  ++reverse : {bs : List A} → (as : List A) → reverse (as ++ bs) ≡ reverse bs ++ reverse as
  ++reverse {bs} = listrec' (sym ++[]) (λ _ as hyp → trans3 (++reverseAux as) (cong2R _++_ hyp) (++assoc (reverse bs)) (cong (_++_ (reverse bs)) (sym (++rev as))))

  revrev' : (as : List A) → reverse (reverse as) ≡ as 
  revrev' = listrec' refl (λ a as hyp → trans2 (cong reverse (reversesEq (a ∷ as))) (++reverse (slowReverse as)) (cong (_∷_ a) (trans (cong reverse (sym (reversesEq as))) hyp)))

  revrev : {as : List A} → reverse (reverse as) ≡ as
  revrev {as} = revrev' as

  reverseEq : {as bs : List A} → reverse as ≡ reverse bs → as ≡ bs 
  reverseEq eq = trans2 (sym revrev) (cong reverse eq) revrev

  ++reverseAuxR : {as bs : List A} → {b : A} → reverse as ++ b ∷ bs ≡ rev [ b ] as ++ bs
  ++reverseAuxR {as} {bs} = trans (assocR {rev [] as}) (cong (flip _++_ bs) (sym (++rev as)))

  ++cancL : {bs cs : List A} → (as : List A) → as ++ bs ≡ as ++ cs → bs ≡ cs
  ++cancL = listrec id (_⋙_ (snd ∘ ∷-inj))

  ++cancR : {bs cs : List A} → (as : List A) → as ++ cs ≡ bs ++ cs → as ≡ bs
  ++cancR {bs} {cs} as eq = reverseEq (++cancL (reverse cs) (trans2 (sym (++reverse as)) (cong reverse eq) (++reverse bs)))

  import Cancellative
  open Cancellative _++_ (λ {as} → ++cancL as) (λ {as} → ++cancR as) public

  ++eq[] : {as bs : List A} → as ++ bs ≡ [] → as ≡ [] × bs ≡ []
  ++eq[] {[]} eq = refl , eq
  ++eq[] {_ ∷ _} ()

  concatMapWrap' : (as : List A) → concat (map wrap as) ≡ as
  concatMapWrap' = listrec refl ∷-congL

  concatMapWrap : {as : List A} → concat (map wrap as) ≡ as
  concatMapWrap {as} = concatMapWrap' as

  concat++ : {bs : List (List A)} → (as : List (List A)) → concat (as ++ bs) ≡ concat as ++ concat bs
  concat++ = listrec' refl (λ a _ hyp → trans (cong (_++_ a) hyp) (assocR {a}))


  -- Prefixes -------------------------------------------------

  data Prefix (ls rs  : List A) : Set where
    prefixB : ls ≡ rs                       → Prefix ls rs
    prefixL : {xs : List A} → ls ++ xs ≡ rs → Prefix ls rs
    prefixR : {xs : List A} → rs ++ xs ≡ ls → Prefix ls rs

  addToPrefix : {ls rs : List A} → (a : A) → Prefix ls rs → Prefix (a ∷ ls) (a ∷ rs)
  addToPrefix a (prefixB refl) = prefixB refl
  addToPrefix a (prefixL refl) = prefixL refl
  addToPrefix a (prefixR refl) = prefixR refl

  comparePrefix : {xs ys : List A} → (ls rs : List A) → ls ++ xs ≡ rs ++ ys → Prefix ls rs
  comparePrefix [] [] eq = prefixB refl
  comparePrefix [] rs eq = prefixL refl
  comparePrefix ls [] eq = prefixR refl
  comparePrefix (a ∷ ls) (_  ∷ rs) eq with ∷-inj eq
  comparePrefix (a ∷ ls) (.a ∷ rs) eq | refl , eqls = addToPrefix a (comparePrefix ls rs eqls)


  -- Suffixes -------------------------------------------------------------

  data Suffix (ls rs : List A) : Set where
    suffixB : ls ≡ rs                       → Suffix ls rs
    suffixL : (xs : List A) → xs ++ ls ≡ rs → Suffix ls rs
    suffixR : (xs : List A) → xs ++ rs ≡ ls → Suffix ls rs

  reverseAux : {bs cs : List A} → (as : List A) → reverse as ++ bs ≡ reverse cs → reverse bs ++ as ≡ cs
  reverseAux {bs} as eq = reverseEq (trans2 (++reverse (reverse bs)) (cong (_++_ (reverse as)) revrev) eq)

  compareSuffix : {xs ys : List A} → (ls rs : List A) → xs ++ ls ≡ ys ++ rs → Suffix ls rs
  compareSuffix {xs} {ys} ls rs eq with comparePrefix (reverse ls) (reverse rs) (trans2 (sym (++reverse xs)) (cong reverse eq) (++reverse ys))
  ... | prefixB eq2 = suffixB (reverseEq eq2)
  ... | prefixL {xls} eq2 = suffixL (reverse xls) (reverseAux ls eq2)
  ... | prefixR {xrs} eq2 = suffixR (reverse xrs) (reverseAux rs eq2)


  -- Element of -----------------------------------------------------------

  infix 3 _∈_ 

  data _∈_ (a : A) : List A → Set where
    hd : {as : List A} → a ∈ a ∷ as
    tl : {b : A} → {as : List A} → a ∈ as → a ∈ b ∷ as

  index : {a : A} → {as : List A} → a ∈ as → ℕ
  index hd = O
  index (tl el) = S (index el)

  data Lookup (as : List A) : ℕ → Set where
    inside  : (a : A) → (el : a ∈ as) → Lookup as (index el)
    outside : (n : ℕ) → Lookup as (length as + n)

  _!_ : (as : List A) → (n : ℕ) → Lookup as n
  [] ! n = outside n
  (a ∷ as) ! O = inside a hd
  (a ∷ as) ! S n with as ! n
  (a ∷ as) ! S .(index el)      | inside b el = inside b (tl el)
  (a ∷ as) ! S .(length as + n) | outside n   = outside n


---------------------------------------------------------------------------

open ListPropsAux public

-- Properties of lists over several sets --

lengthMap : {A B : Set} → {f : A → B} → (as : List A) → length (map f as) ≡ length as
lengthMap = listrec refl (cong S)

map++ : {A B : Set} → {f : A → B} → {as₂ : List A} → (as₁ : List A) → map f (as₁ ++ as₂) ≡ map f as₁ ++ map f as₂
map++ = listrec refl ∷-congL

mapmap : {A B C : Set} → {f : B → C} → {g : A → B} → (as : List A) → map f (map g as) ≡ map (f ∘ g) as
mapmap = listrec refl (∷-cong refl)
