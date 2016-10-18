{-# OPTIONS --type-in-type  #-}

open import NeilPrelude

module When where

---------------------------------------------------------------------

data When (A : Set) : Set where
  Now     : A     → When A
  Soon    : A     → When A
  NowSoon : A → A → When A

---------------------------------------------------------------------

mapWhen : {A B : Set} → (A → B) → When A → When B
mapWhen f (Now  a)      = Now  (f a)
mapWhen f (Soon a)      = Soon (f a)
mapWhen f (NowSoon a b) = NowSoon (f a) (f b)

mapWhen2 : {A B C : Set} → (A → B → C) → A → B → When A → When B → When C
mapWhen2 f a₀ b₀ (Now a₁)        (Now b₁)         =  Now (f a₁ b₁)
mapWhen2 f a₀ b₀ (Now a₁)        (Soon b₂)        =  NowSoon (f a₁ b₀) (f a₁ b₂)
mapWhen2 f a₀ b₀ (Now a₁)        (NowSoon b₁ b₂)  =  NowSoon (f a₁ b₁) (f a₁ b₂)
mapWhen2 f a₀ b₀ (Soon a₂)       (Now b₁)         =  NowSoon (f a₀ b₁) (f a₂ b₁)
mapWhen2 f a₀ b₀ (Soon a₂)       (Soon b₂)        =  Soon (f a₂ b₂)
mapWhen2 f a₀ b₀ (Soon a₂)       (NowSoon b₁ b₂)  =  NowSoon (f a₀ b₁) (f a₂ b₂)
mapWhen2 f a₀ b₀ (NowSoon a₁ a₂) (Now b₁)         =  NowSoon (f a₁ b₁) (f a₂ b₁)
mapWhen2 f a₀ b₀ (NowSoon a₁ a₂) (Soon b₂)        =  NowSoon (f a₁ b₀) (f a₂ b₂)
mapWhen2 f a₀ b₀ (NowSoon a₁ a₂) (NowSoon b₁ b₂)  =  NowSoon (f a₁ b₁) (f a₂ b₂)

mergeWhen : {A B C : Set} → (A → B → C) → (A → C) → (B → C) → When A → When B → When C
mergeWhen f g h (Now a₁)        (Now b₁)         =  Now (f a₁ b₁)
mergeWhen f g h (Now a₁)        (Soon b₂)        =  NowSoon (g a₁) (h b₂)
mergeWhen f g h (Now a₁)        (NowSoon b₁ b₂)  =  NowSoon (f a₁ b₁) (h b₂)
mergeWhen f g h (Soon a₂)       (Now b₁)         =  NowSoon (h b₁) (g a₂)
mergeWhen f g h (Soon a₂)       (Soon b₂)        =  Soon (f a₂ b₂)
mergeWhen f g h (Soon a₂)       (NowSoon b₁ b₂)  =  NowSoon (h b₁) (f a₂ b₂)
mergeWhen f g h (NowSoon a₁ a₂) (Now b₁)         =  NowSoon (f a₁ b₁) (g a₂)
mergeWhen f g h (NowSoon a₁ a₂) (Soon b₂)        =  NowSoon (g a₁) (f a₂ b₂)
mergeWhen f g h (NowSoon a₁ a₂) (NowSoon b₁ b₂)  =  NowSoon (f a₁ b₁) (f a₂ b₂)

---------------------------------------------------------------------

valHence : {A : Set} → When A → A
valHence (Now a)       = a
valHence (Soon b)      = b
valHence (NowSoon a b) = b

valNow : {A : Set} → When A → Maybe A
valNow (Now a)       = just a
valNow (Soon b)      = nothing
valNow (NowSoon a b) = just a

valSoon : {A : Set} → When A → Maybe A
valSoon (Now a)       = nothing
valSoon (Soon b)      = just b
valSoon (NowSoon a b) = just b

---------------------------------------------------------------------

preWhen : {A : Set} → (A → A → A) → When A → When A
preWhen f (Now a)       = Soon a
preWhen f (Soon b)      = Soon b
preWhen f (NowSoon a b) = Soon (f a b)

---------------------------------------------------------------------

nowAndMaybeSoon : {A : Set} → A → Maybe A → When A
nowAndMaybeSoon a nothing  = Now a
nowAndMaybeSoon a (just b) = NowSoon a b

maybeNowAndSoon : {A : Set} → Maybe A → A → When A
maybeNowAndSoon nothing b  = Soon b
maybeNowAndSoon (just a) b = NowSoon a b

nowsoon : {A : Set} → Maybe A → Maybe A → Maybe (When A)
nowsoon (just a) mb      = just (nowAndMaybeSoon a mb)
nowsoon nothing (just b) = just (Soon b)
nowsoon nothing  nothing = nothing

---------------------------------------------------------------------
