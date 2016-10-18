{-# OPTIONS --type-in-type  #-}

open import NeilPrelude

module RealTime where

open import PosReal public

----------------------------------------------

Time : Set
Time = ℜ₀

Time⁺ : Set
Time⁺ = ℜ⁺ 

Δt = Time⁺

CurrentTime     = Time
EventTime       = Time
ReleaseTime     = Time
SampleTime      = Time
StartTime       = Time
TimeAfterEvent  = Time
TimeBeforeEvent = Time

CurrentTime⁺    = Time⁺
ReleaseTime⁺    = Time⁺
SampleTime⁺     = Time⁺
EventTime⁺      = Time⁺

---------------------------------------------

