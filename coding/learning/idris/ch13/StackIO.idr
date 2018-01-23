import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
  Push   : Integer -> StackCmd () height (S height)
  Pop    : StackCmd Integer (S height) height
  Top    : StackCmd Integer (S height) (S height) -- omitting S would not enforce to have at least one element in the stack

  GetStr : StackCmd String height height
  PutStr : String -> StackCmd () height height

  Pure   : t -> StackCmd t height height
  (>>=)  : StackCmd a h1 h2 -> (a -> StackCmd b h2 h3) -> StackCmd b h1 h3

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 + val2)

doSub : StackCmd () (S (S height)) (S height)
doSub = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 - val2)

doMult : StackCmd () (S (S height)) (S height)
doMult = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 * val2)

doNegate : StackCmd () (S height) (S height)
doNegate = do
  val <- Pop
  Push (-val)

doDuplicate : StackCmd () (S height) (S (S height))
doDuplicate = do
  val <- Top
  Push (val)

runStack : (st : Vect inHeight Integer) -> StackCmd t inHeight outHeight -> IO (t, Vect outHeight Integer)
runStack st GetStr = do
  str <- getLine
  pure (str, st)
runStack st (PutStr str) = do
  putStr str
  pure ((), st)
runStack st (Push x) = pure ((), x :: st)
runStack (x :: st) Pop = pure (x, st)
runStack (x :: st) Top = pure (x, x :: st)
runStack st (Pure x) = pure (x, st)
runStack st (cmd >>= cont) = do
  (t, st') <- runStack st cmd
  runStack st' (cont t)

data StackIO : Nat -> Type where
  Do : StackCmd a h1 h2 -> (a -> Inf (StackIO h2)) -> StackIO h1

namespace StackDo
  (>>=) : StackCmd a h1 h2 -> (a -> Inf (StackIO h2)) -> StackIO h1
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run Dry st p = pure ()
run (More f) st (Do cmd cont) = do
  (t, st') <- runStack st cmd 
  run f st' (cont t)

data StkInput = Number Integer 
              | Add
              | Sub
              | Mult
              | Neg
              | Dis
              | Dup

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "+" = Just Add
strToInput "-" = Just Sub
strToInput "*" = Just Mult
strToInput "neg" = Just Neg
strToInput "dis" = Just Dis
strToInput "dup" = Just Dup
strToInput str 
  = case all isDigit (unpack str) of
         True => Just (Number (cast str))
         False => Nothing

mutual
  tryAdd : StackIO height
  tryAdd {height = (S (S h))} = do
    doAdd
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryAdd = do
    PutStr "Fewer than two items on the stack\n"
    stackCalc

  trySub : StackIO height
  trySub {height = (S (S h))} = do
    doSub
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  trySub = do
    PutStr "Fewer than two items on the stack\n"
    stackCalc

  tryMult : StackIO height
  tryMult {height = (S (S h))} = do
    doMult
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryMult = do
    PutStr "Fewer than two items on the stack\n"
    stackCalc

  tryNeg : StackIO height
  tryNeg {height = (S h)} = do
    doNegate
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryNeg = do
    PutStr "No item on the stack\n"
    stackCalc

  tryDis : StackIO height
  tryDis {height = (S h)} = do
    result <- Pop
    PutStr ("Discarded: " ++ show result ++ "\n")
    stackCalc
  tryDis = do
    PutStr "No item on the stack\n"
    stackCalc

  tryDup : StackIO height
  tryDup {height = (S h)} = do
    doDuplicate
    val <- Top
    PutStr ("Duplicated: " ++ show val ++ "\n")
    stackCalc
  tryDup = do
    PutStr "No item on the stack\n"
    stackCalc

  stackCalc : StackIO height
  stackCalc = do
    PutStr "> "
    input <- GetStr
    case strToInput input of
         Nothing  => do
           PutStr "Invalid input\n"
           stackCalc
         Just (Number x) => do
           Push x
           stackCalc
         Just Add => tryAdd
         Just Sub => trySub
         Just Mult => tryMult
         Just Neg => tryNeg
         Just Dis => tryDis
         Just Dup => tryDup

partial
main : IO ()
main = run forever [] stackCalc