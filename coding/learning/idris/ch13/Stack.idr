import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
  Push  : Integer -> StackCmd () height (S height)
  Pop   : StackCmd Integer (S height) height
  Top   : StackCmd Integer (S height) (S height) -- omitting S would not enforce to have at least one element in the stack

  Pure  : t -> StackCmd t height height
  (>>=) : StackCmd a h1 h2 -> (a -> StackCmd b h2 h3) -> StackCmd b h1 h3

runStack : (st : Vect inHeight Integer) -> StackCmd t inHeight outHeight -> (t, Vect outHeight Integer)
runStack st (Push x) = ((), x :: st)
runStack (x :: st) Pop = (x, st)
runStack (x :: st) Top = (x, x :: st)
runStack st (Pure x) = (x, st)
runStack st (cmd >>= cont) 
  = let (t, st') = runStack st cmd in
        runStack st' (cont t)

testAdd : StackCmd Integer 0 0
testAdd = do
  Push 10
  Push 20
  val1 <- Pop
  val2 <- Pop
  Pure (val1 + val2)

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 + val2)
