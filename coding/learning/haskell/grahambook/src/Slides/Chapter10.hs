-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 10 (Declaring Types and Classes)

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n) 

-- NOTE: defined by me
showNat :: Nat -> String
showNat (Zero) = "Zero"
showNat (Succ n) = "Succ (" ++ (showNat n) ++ ")"

instance Show Nat where
  show n = showNat n


data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

-- (1) Using recursion and the function add, define a function that multiplies two natural numbers.
-- NOTE: Zero times a Nat is always Zero. The implementation works by adding m times the natural number n together
mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n

-- (2) Define a suitable function fold for expressions, and give a few examples of its use.
-- unclear what to do?

-- (3) A binary tree is complete if the two sub-trees of every node are of equal size.  Define a function that decides if a binary tree is complete.
completeTree :: Tree a -> Bool
completeTree (Leaf l) = True
completeTree (Node l v r) = (countSubTreeDepth l) == (countSubTreeDepth r)

countSubTreeDepth :: Tree a -> Int
countSubTreeDepth (Leaf l) = 0
countSubTreeDepth (Node l v r) = 1 + (countSubTreeDepth l) + (countSubTreeDepth r)

-- NOTE: t1 is complete
t1 :: Tree Int
t1 = Node (Node (Leaf 1) 3 (Leaf 4)) 5
          (Node (Leaf 6) 7 (Leaf 9))

-- NOTE: t2 is NOT complete
t2 :: Tree Int
t2 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Leaf 7)
