-- Calculator example from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

import Parsing
import System.IO

-- Expression parser

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> do symbol "/"
                  t <- term
                  return (f `div` t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> integer

-- IO utilities

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: (Int,Int) -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Calculator

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
             standard = "qcd=123+456-789*0()/"
             extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs 
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval xs
             | elem c "cC"        = clear
             | otherwise          = press c xs
 
quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
             [(n,[])] -> calc (show n)
             _        -> do beep
                            calc xs
 
clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
