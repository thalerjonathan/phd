-- The Set of the Boolean Truth-Values
data Bool : Set where
  true : Bool
  false : Bool

-- Exercise: Define some more truth functions, such as conjunction and implication.
_conj_ : Bool → Bool → Bool
true conj true = true
_ conj _ = false

_impl_ : Bool → Bool → Bool
true impl false = false
_ impl _ = true


-- The Set of the inductive Natural Numbers
data Nat : Set where
  zero : Nat
  succ : Nat -> Nat

{- Exercise: Write the cut-off subtraction function - the function on natural numbers, 
which returns 0 if the second argument is greater than or equal to the first. 
Also write some more numerical functions like < or <=.
-}

_subCut_ : Nat → Nat → Nat
zero subCut zero = zero
zero subCut n = zero
n subCut zero = n
(succ n) subCut (succ m) = n subCut m

_lt_ : Nat → Nat → Bool
zero lt zero = false
zero lt n = true
n lt zero = false
(succ n) lt (succ m) = n lt m

_le_ : Nat → Nat → Bool
zero le zero = true
zero le n = true
n le zero = false
(succ n) le (succ m) = n le m


-- The primitive recursion combinator for natural numbers of Goedel System T
-- NOTE BY ME: reminds one of foldr of Haskell
-- NOTE BY ME: p is the value returned in the base-case, h is the step-function applied at each recursion step and n is the natural number on which to perform the recursion
-- NOTE BY ME: the step-function receives two values: x and y where x is the current value of n which is the predecessor of the previous n in each recursion and y which is the accumulated value over all recursions starting with the base case
if_then_else_ : {C : Set} → Bool → C → C → C
if true  then x else y = x
if false then x else y = y

natrec : {C : Set} → C → (Nat → C → C) → Nat → C
natrec p h zero = p
natrec p h (succ n) = h n (natrec p h n)

-- Exercise: Define all functions previously given in the text in Goedel System T
-- NOTE BY ME: plus & mult is already given in the text => implement subCut, lt and le
plus : Nat -> Nat -> Nat
plus n m = natrec m (\x y -> succ y) n

mult : Nat -> Nat -> Nat
mult n m = natrec zero (\x y -> plus y m) n

pred : Nat → Nat
pred zero = zero
pred (succ n) = n

pred' : Nat → Nat
-- NOTE BY ME: using natrec one uses the fact that in natrec always the predecessor of the current n is calculated and passed in step to the step-function. One just takes the first pred and then forgets the rest which is passed in acc to the step-function. Starting conditions do not matter: either zero or n
pred' n = natrec zero (\step acc -> step) n

subCut' : Nat → Nat → Nat
subCut' n m = natrec m (\step acc -> step) n
