module Main where

import Interpreter

-- prog :: Expr
-- prog = TryHandle (Perform Unit) "x" (Print (Var "x") \>> Perform (Var "x"))
-- you have to bake the parent handler into the new handler when you do a try/handle so the handler doesn't try to use itself if it performs an effect
prog :: Expr
prog = TryHandle (Perform Unit) "x" (Print (Var "x") \>> Perform (Var "x"))

main :: IO ()
main = print =<< runEvalExpr prog
