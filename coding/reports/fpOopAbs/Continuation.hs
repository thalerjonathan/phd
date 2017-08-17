module Examples where

main :: IO ()
main =
    do
        let c = cont 42
        let (os, c') = runCont 5 10 c
        print os

        let (os', c'') = runCont 10 20 c'
        print os'

newtype Cont i o = Cont (i -> (o, Cont i o))

runCont :: Int -> i -> Cont i o -> ([o], Cont i o)
runCont 0 _ c = ([], c)
runCont n i (Cont f) = (o : os, c'')
    where
        (o, c') = f i
        (os, c'') = runCont (n-1) i c'

cont :: Int -> Cont Int String
cont i' = Cont (\i -> ("cont: " ++ show (i' + i), cont (i' + 1)))