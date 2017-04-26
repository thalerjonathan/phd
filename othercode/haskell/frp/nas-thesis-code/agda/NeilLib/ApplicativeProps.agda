{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Equational

module ApplicativeProps (F : Set → Set)

       (fmap   : {A B : Set} → (A → B) → F A → F B)
       (pure   : {A : Set} → A → F A)
       (_<*>_  : {A B : Set} → F (A → B) → F A → F B) 

-- applicative functor laws

       (fmapLaw         : {A B : Set} → {g : A → B} → {fa : F A} → fmap g fa ≡ pure g <*> fa)
       (identityLaw     : {A : Set} → {fa : F A} → pure id <*> fa ≡ fa)
       (compositionLaw  : {A B C : Set} → {fa : F A} → {fbc : F (B → C)} → {fab : F (A → B)} → 
                          ((pure (_∘_) <*> fbc) <*> fab) <*> fa ≡ fbc <*> (fab <*> fa))
       (homomorphismLaw : {A B : Set} → {f : A → B} → {a : A} → pure f <*> pure a ≡ pure (f a))
       (interchangeLaw  : {A B : Set} → {fab : F (A → B)} → {a : A} →
                          fab <*> pure a ≡ pure (λ g → g a) <*> fab)

where

functorLaw1 : {A : Set} → {fa : F A} → fmap id fa ≡ fa
functorLaw1 = trans fmapLaw identityLaw

functorLaw2 : {A B C : Set} → {f : A → B} → {g : B → C} → {fa : F A} → 
              fmap (g ∘ f) fa ≡ fmap g (fmap f fa)
functorLaw2 {_} {_} {_} {f} {g} {fa} =

   fmap (g ∘ f) fa
                                                ≡⟨ fmapLaw ⟩
   pure (g ∘ f) <*> fa
                                                ≡⟨ cong (flip _<*>_ fa) (sym homomorphismLaw) ⟩
   (pure (_∘_ g) <*> pure f) <*> fa
                                                ≡⟨ cong (flip _<*>_ fa ∘ flip _<*>_ (pure f)) (sym homomorphismLaw) ⟩
   ((pure _∘_ <*> pure g) <*> pure f) <*> fa
                                                ≡⟨ compositionLaw ⟩
   pure g <*> (pure f <*> fa)
                                                ≡⟨ cong (_<*>_ (pure g)) (sym fmapLaw) ⟩
   pure g <*> fmap f fa
                                                ≡⟨ sym fmapLaw ⟩
   fmap g (fmap f fa)
                                                QED


pointedLaw : {A B : Set} → {f : A → B} → {a : A} → fmap f (pure a) ≡ pure (f a)
pointedLaw {_} {_} {f} {a} =  fmap f (pure a)
                                              ≡⟨ fmapLaw ⟩
                              pure f <*> pure a
                                              ≡⟨ homomorphismLaw ⟩
                              pure (f a)
                                              QED 

import Applicative
open Applicative F fmap pure _<*>_

import PointedProps
open PointedProps F fmap pure functorLaw1 functorLaw2 pointedLaw public

----------------------------------------------

-- All Applicative Functors are Static Arrows

-- arrowLaw1 : {A B : Set} → {f : F (A → B)} → fmap (_⋙_) (pure id) <*> f ≡ f
-- arrowLaw1 {_} {_} {f} = fmap _⋙_ (pure id) <*> f
--                                    ≡⟨ cong2R _<*>_ pointedLaw ⟩
--                         pure id <*> f
--                                    ≡⟨ identityLaw ⟩
--                         f QED

-- arrowLaw3 : {A B C D : Set} → {f : F (A → B)} → {g : F (B → C)} → {h : F (C → D)} →
--        fmap (_⋙_) (fmap (_⋙_) f <*> g) <*> h ≡
--        fmap (_⋙_) f <*> (fmap (_⋙_) g <*> h)
-- arrowLaw3 {_} {_} {_} {_} {f} {g} {h} = {!!}

-- arrowLaw4 : {A B C : Set} → {f : A → B} → {g : B → C} → pure (g ∘ f) ≡ fmap (_⋙_) (pure f) <*> pure g
-- arrowLaw4 {_} {_} {_} {f} {g} = pure (g ∘ f)
--                                               ≡⟨ sym homomorphismLaw ⟩
--                                 pure (_⋙_ f) <*> pure g
--                                               ≡⟨ cong2R _<*>_ (sym pointedLaw) ⟩
--                                 fmap _⋙_ (pure f) <*> pure g
--                                               QED

-- arrowLaw5 : {A B X : Set} → {f : A → B} → fmap (first {_} {X}) (pure f) ≡ pure (first f)
-- arrowLaw5 = pointedLaw

-- arrowLaw6 : {A B C D : Set} → {f : F (A → B)} → {g : F (B → C)} →
--        fmap (first {_} {D}) (fmap (_⋙_) f <*> g) ≡ fmap (_⋙_) (fmap first f) <*> fmap first g
-- arrowLaw6 {_} {_} {_} {D} {f} {g} = fmap (first {_} {D}) (fmap _⋙_ f <*> g)
--                                              ≡⟨ fmapLaw ⟩
--                                     pure (first {_} {D}) <*> (fmap _⋙_ f <*> g)
--                                              ≡⟨ cong2L _<*>_ (cong2R _<*>_ fmapLaw) ⟩
--                                     pure (first {_} {D}) <*> ((pure _⋙_ <*> f) <*> g)
--                                              ≡⟨ {!!} ⟩
--                                     (pure (_⋙_ ∘ first) <*> f) <*> (pure first <*> g)
--                                              ≡⟨ cong2R _<*>_ (sym fmapLaw) ⟩
--                                     fmap (_⋙_ ∘ first) f <*> (pure first <*> g)
--                                              ≡⟨ cong2L _<*>_ (sym fmapLaw) ⟩
--                                     fmap (_⋙_ ∘ first) f <*> fmap first g
--                                              ≡⟨ cong2R _<*>_ functorLaw2 ⟩
--                                     fmap _⋙_ (fmap first f) <*> fmap first g
--                                              QED

-- arrowLaw7 : {A B C D : Set} → {f : F (A → B)} → {g : C → D} →
--               fmap (_⋙_) (fmap first f) <*> pure (second g) ≡
--               fmap (_⋙_) (pure (second g)) <*> fmap first f
-- arrowLaw7 = {!!}

-- arrowLaw8 : {A B C : Set} → {f : F (A → B)} →
--               fmap (_⋙_) (fmap (first {_} {C}) f) <*> pure fst ≡
--               fmap (_⋙_) (pure fst) <*> f
-- arrowLaw8 = {!!}

-- arrowLaw9 : {A B C D : Set} → {f : F (A → B)} →
--               fmap (_⋙_) (fmap (first {_} {C}) (fmap (first {_} {D}) f)) <*> pure ×-assocR ≡
--               fmap (_⋙_) (pure ×-assocR) <*> fmap first f
-- arrowLaw9 = {!!}

-- staticArrowLaw1 : {A B : Set} → {f : F (A → B)} →
--                     fmap (_⋙_) (pure (_,_ unit)) <*>
--                     (fmap (_⋙_) (fmap first (fmap const f)) <*> pure ×-apply)
--                     ≡ f
-- staticArrowLaw1 = {!!}

-- staticArrowLaw2 : {A B : Set} → {f : F (Unit → A → B)} →
--                     fmap const
--                     (fmap (_⋙_) (pure (_,_ unit)) <*>
--                      (fmap (_⋙_) (fmap first f) <*> pure ×-apply))
--                     ≡ f
-- staticArrowLaw2 = {!!}


-- import StaticArrowProps
-- open StaticArrowProps Arr pure (fmap2 _⋙_) (fmap first) (fmap const) force'
--                       arrowLaw1 arrowLaw3 arrowLaw4 arrowLaw5 arrowLaw6 arrowLaw7 arrowLaw8 arrowLaw9 staticArrowLaw1 staticArrowLaw2
