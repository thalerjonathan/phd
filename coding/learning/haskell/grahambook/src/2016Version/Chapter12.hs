{- 1. Define an instance of the Functor class for the following type of binary trees that have data in their nodes: -}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Leaf)

func :: Int -> String
func a = show a

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap f Leaf = Leaf
   fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

{- 2. Complete the following instance declaration to make the partially applied function type (a ->) into a functor
instance Functor ((->) a) where
...

Hint: first write down the type of fmap, and then think if you already know a library function that has this type.

instance Functor ((->) a) where
    -- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
    fmap f g = (\x -> f (g x))
    fmap = (.)

-}

{- 3. Define an instance of the Applicative class for the type (a ->). If you are familiar with combinatory logic,
you might recognize pure and <*> for this type as being the well-known K and S combinators. -}

{- 4. There may be more than one way to make a parameterised type into an applicative functor. For example,
the library Control.Applicative provide an alternative 'zippy' instance for lists, in which the function pure
makes an infinite list of copies of its argument, and the operator <*> applies each argument function to the
corresponding argument value at the same position. Complete the following declarations that implement this idea -}

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z (map (\(g, x) -> g x) (zip gs xs))

-- The ZipList wrapper around the list type is required because each type can only have at most one instance declaration for a given class

{- 5. Work out the types for the variables in the four applicative laws. -}

{- 6. Define an instance of the Monad class for the type (a ->) -}

{- 7. Given the following type of expressions, that contain cariables of some type a, show how to make this type into
 instances of the Functor, Applicative and Monad classes. With the aid of an example, explain what the >>= operator
 for this class does -}
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var a) = Var (f a)
    fmap f (Val x) = Val x          -- NOTE: stays the same, won't be transformed because fixed to Int and not type a
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a = Var a
    pure a = Var a                  -- NOTE: transforming to Var a is the obvious choice. Val Int is not applicable because Int != a and Add would lack an expression

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    Var f <*> Var a = Var (f a)
    Var f <*> Val x = Val x         -- NOTE: stays the same, won't be transformed because fixed to Int and not type a
    Var f <*> (Add l r) = Add (fmap f l) (fmap f r)

instance Monad Expr where
    -- return :: a = Var a
    return = pure

    -- >>= :: Expr a -> (a -> Expr b) -> Expr b
    Var a >>= f = f a
    Val x >>= f = Val x
    (Add l r) >>= f = Add (l >>= f) (r >>= f)
    --(Add l r) >>= f =

eval :: Expr Int -> Int
eval (Var x) = x
eval (Val x) = x
eval (Add l r) = eval l + eval r

exprVal :: Expr Int
exprVal = Add (Val 1) (Val 2)

exprVar :: Expr Int
exprVar = Add (Var 1) (Var 2)

{-
test :: Expr()
test = do
    let expr = Add (Val 1) (Val 2)
    a <- expr (+)
-}

{- 8. Rather than making a parameterized type into instances of the Functor, Applicative and Monad classes in this order, in practice
 it is sometimes simpler to define the functor and applicative instances in terms of the monad instance, relying on the fact
 that the order in which declarations are made is not important in Haskell. Complete the missing parts in the following
 declarations for the ST type using the do notation

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do ...

instance Applicative ST where
    -- pure :: a -> ST a
    pure a = S (\s -> (x, s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do ...

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
        let (x, s') = app st s in app (f x) s')
-}