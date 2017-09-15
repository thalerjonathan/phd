module EqInstances where

data TisAnInteger =
    TisAn Integer

data TwoIntegers =
    Two Integer Integer

data StringOrInt =
    TisAnInt Int
    | TisAString String

data Pair a =
    Pair a a

data Tuple a b =
    Tuple a b

data Which a =
    ThisOne a
    | ThatOne a

data EitherOr a b =
    Hello a
    | Goodbye b


instance Eq TisAnInteger where
    (TisAn i1) == (TisAn i2) = i1 == i2

instance Eq TwoIntegers where
    (Two t1i1 t1i2) == (Two t2i1 t2i2) = t1i1 == t2i1 && t1i2 == t2i2

instance Eq StringOrInt where
    (TisAnInt i1) == (TisAnInt i2) = i1 == i2
    (TisAString s1) == (TisAString s2) = s1 == s2
    _ == _ = False

instance Eq a => Eq (Pair a) where
    (Pair p1a1 p1a2) == (Pair p2a1 p2a2) = p1a1 == p2a1 && p1a2 == p2a2

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (Tuple a1 b1) == (Tuple a2 b2) = a1 == a2 && b1 == b2

instance (Eq a) => Eq (Which a) where
    (ThisOne a1) == (ThisOne a2) = a1 == a2
    (ThatOne a1) == (ThatOne a2) = a1 == a2
    _ == _ = False

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (Hello a1) == (Hello a2) = a1 == a2
    (Goodbye b1) == (Goodbye b2) = b1 == b2
    _ == _ = False

main :: IO ()
main = 
    do
        let tai1 = TisAn 42 
        let tai2 = TisAn 42
        let tai3 = TisAn 0

        print $ tai1 == tai2
        print $ tai1 == tai3
        putStrLn ""

        let ti1 = Two 0 1
        let ti2 = Two 0 2
        let ti3 = Two 1 1
        let ti4 = Two 0 1

        print $ ti1 == ti2
        print $ ti1 == ti3
        print $ ti1 == ti4
        putStrLn ""

        let s1 = TisAString "Hello"
        let s2 = TisAString "World"
        let i1 = TisAnInt 0
        let i2 = TisAnInt 1

        print $ s1 == i1
        print $ s1 == s2
        print $ s1 == s1
        print $ i1 == s1
        print $ i1 == i2
        print $ i1 == i1
        putStrLn ""

        let pi1 = Pair 0 1
        let ps1 = Pair "Hello" "World"
        -- NOTE: this is detected already rejected the compiler
        -- print $ p1 == p2
        print $ pi1 == pi1
        print $ ps1 == ps1
        putStrLn ""

        print $ (Tuple "Hello" "World") == (Tuple "hello" "world")
        print $ (Tuple 0 "World") == (Tuple 0 "World")
        putStrLn ""

        print $ (ThisOne 0) == (ThatOne 0)
        print $ (ThisOne 42) == (ThisOne 42)
        print $ (ThatOne 42) == (ThisOne 42)
        print $ (ThatOne 42) == (ThatOne 42)
        putStrLn ""

        let eoh1 = Hello "World" :: EitherOr String String
        let eoh2 = Hello "world" :: EitherOr String String

        let eog1 = Goodbye "World" :: EitherOr String String
        let eog2 = Goodbye "world" :: EitherOr String String

        print $ eoh1 == eoh1
        print $ eog1 == eog1
        print $ eoh1 == eog1
        print $ eoh2 == eog2
        print $ eoh1 == eog2
        print $ eoh2 == eog1