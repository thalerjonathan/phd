-- Expression example from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

import Parsing

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
             [(n,[])]  -> n
             [(_,out)] -> error ("Unused input " ++ out)
             []        -> error "Invalid input"
