module Main

import Effects
import Effect.StdIO
import Effect.Random
import Effect.State
import Effect.Exception

data Expr = Val Integer
          | Add Expr Expr
          | Var String
          | Random Integer

Vars : Type
Vars = List (String, Integer)

eval : Expr -> Eff Integer [EXCEPTION String, RND, STATE Vars]
eval (Val x) = pure x
eval (Add x y) = do
  x' <- eval x
  y' <- eval y
  pure (x' + y')
eval (Var name) = do
  case lookup name !get of
       Nothing => raise ("Error: variable " ++ name ++ " not defined!")
       Just val  => pure val
eval (Random upperBound) = rndInt 0 upperBound

runEval : Vars -> Expr -> Maybe Integer
runEval env expr = runInit [(), 42, env] (eval expr)

main : IO ()
main = do
  let env = [("a", 42)]
  let expr = Add (Var "a") (Var "b")
  let res = runEval env expr
  printLn res