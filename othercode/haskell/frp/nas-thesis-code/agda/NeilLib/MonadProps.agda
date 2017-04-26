{-# OPTIONS --type-in-type #-}

open import NeilPrelude
open import Equational

module MonadProps (M : Set → Set)

       (_>>='_ : {A B : Set} → M A → (A → M B) → M B)
       (ret    : {A : Set} → A → M A)

-- monad laws

       (1stLaw : {A B : Set} → {a : A} → {f : A → M B} → ret a >>=' f ≡ f a)
       (2ndLaw : {A : Set} → {ma : M A} → ma >>=' ret ≡ ma)
       (3rdLaw : {A B C : Set} → {f : A → M B} → {g : B → M C} →
                 (ma : M A) → (ma >>=' f) >>=' g ≡ ma >>=' (λ b → f b >>=' g))

where

import Monad
open Monad M _>>='_ ret


--- Applicative Functor Instance ------------------------------------------


applicativeIdentLaw : {A : Set} → {fa : M A} → return id >>= (λ f → fa >>= return ∘ f) ≡ fa
applicativeIdentLaw = trans 1stLaw 2ndLaw

applicativeHomoLaw : {A B : Set} → {f : A → B} → {a : A} → (return f >>= λ g → return a >>= λ b → return (g b)) ≡ return (f a)
applicativeHomoLaw = trans 1stLaw 1stLaw

module ApplicativeLaws (ext : Extensionality) where

    applicativeCompLaw : {A B C : Set} → {fa : M A} → {fbc : M (B → C)} → {fab : M (A → B)} → 
           ((ret _∘_ >>= (λ f → fbc >>= return ∘ f)) >>= (λ f → fab >>= return ∘ f)) >>= (λ g → fa >>= return ∘ g)
         ≡
           fbc >>= (λ f → (fab >>= λ g → fa >>= λ a → return (g a)) >>= λ b → return (f b))

    applicativeCompLaw {_} {_} {_} {fa} {fbc} {fab} =

      ((ret _∘_ >>= (λ f → fbc >>= return ∘ f)) >>= (λ f → fab >>= return ∘ f)) >>= (λ g → fa >>= return ∘ g)

                  ≡⟨ 3rdLaw (ret _∘_ >>= λ f → fbc >>= return ∘ f) ⟩

      (ret _∘_ >>= (λ f → fbc >>= return ∘ f)) >>= (λ b → (fab >>= return ∘ b) >>= (λ g → fa >>= return ∘ g))

                  ≡⟨ cong (flip _>>=_ (λ b → fab >>= return ∘ b >>= (λ g → fa >>= return ∘ g))) 1stLaw ⟩

      (fbc >>= return ∘ _∘_) >>= (λ b → (fab >>= return ∘ b) >>= (λ g → fa >>= return ∘ g))

                  ≡⟨ 3rdLaw fbc ⟩

      fbc >>= (λ f → return (_∘_ f) >>= (λ b → (fab >>= return ∘ b) >>= (λ g → fa >>= return ∘ g)))

                  ≡⟨ cong (_>>=_ fbc) (ext (λ f → 
                                       return (_∘_ f) >>= (λ b → fab >>= return ∘ b >>= (λ g → fa >>= return ∘ g))
                                                  ≡⟨ 1stLaw ⟩
                                       fab >>= return ∘ (_∘_ f) >>= (λ g → fa >>= return ∘ g)
                                                  ≡⟨ 3rdLaw fab ⟩
                                       fab >>= (λ h → return (f ∘ h) >>= (λ g → fa >>= return ∘ g) )
                                                  ≡⟨ cong (_>>=_ fab) (ext (λ h →
                                                                                 return (f ∘ h) >>= (λ g → fa >>= return ∘ g)
                                                                                             ≡⟨ 1stLaw ⟩
                                                                                 fa >>= return ∘ f ∘ h
                                                                                             ≡⟨ cong (_>>=_ fa) (ext (λ _ → sym 1stLaw) ) ⟩
                                                                                 fa >>= (λ g → return (h g) >>= return ∘ f)
                                                                                             ≡⟨ sym (3rdLaw fa) ⟩
                                                                                 (fa >>= return ∘ h) >>= return ∘ f
                                                                                             QED
                                                                      )) ⟩
                                       fab >>= (λ h → (fa >>= return ∘ h) >>= return ∘ f)
                                                  ≡⟨ sym (3rdLaw fab) ⟩
                                       fab >>= (λ h → fa >>= return ∘ h) >>= return ∘ f
                                                  QED
                                      )) ⟩

      fbc >>= (λ f → (fab >>= (λ h → fa >>= return ∘ h)) >>= return ∘ f)

                                     QED

    
    applicativeInterLaw : {A B : Set} → {fab : M (A → B)} → {a : A} →
                          fab >>= (λ f → ret a >>= (λ b → ret (f b))) ≡ ret (applyTo a) >>= (λ f → fab >>= (λ b → ret (f b)))
    applicativeInterLaw {_} {_} {fab} {a} =     fab >>= (λ f → ret a >>= (λ g → ret (f g)))
                                                                                                  ≡⟨ cong (_>>=_ fab) (ext (λ _ → 1stLaw)) ⟩
                                                fab >>= (λ g → ret (g a))
                                                                                                  ≡⟨ sym 1stLaw ⟩
                                                ret (applyTo a) >>= (λ f → fab >>= (λ g → ret (f g)))
                                                                                                  QED
    
    import ApplicativeProps
    open ApplicativeProps M liftM return ap refl applicativeIdentLaw applicativeCompLaw applicativeHomoLaw applicativeInterLaw public
