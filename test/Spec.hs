import Test.Hspec

import Interpreter

main :: IO ()
main = hspec $ do
    describe "effect handling" $ do
        it "handles effect decorators" $ do
            let prog = TryHandle (Perform Unit) "x" (Print (Var "x") \>> Perform (Var "x"))
                result = runInterpreter (evalExpr prog)
            result `shouldBe` (Left "Unhandled effect: ()", [VUnit])
        it "handles high order effect decorators" $ do
            let prog = lets
                    [ ("decorate", "run" \->
                        TryHandle
                            (Var "run" \$ Unit)
                            "e" (Print (Var "e") \>> Perform (Var "e")))
                    ]
                    (TryHandle (Var "decorate" \$ ("_" \-> (Perform (Int 1) \+ Int 2))) "e" (Var "e"))
                result = runInterpreter (evalExpr prog)
            result `shouldBe` (Right (VInt 3), [VInt 1])
        it "selects inner-most handler" $ do
            let prog = lets
                    [ ("add", "_" \-> TryHandle (Perform (Int 1) \+ Int 2) "e" (Var "e")) ]
                    (TryHandle (Var "add" \$ Unit) "_" (Int 44))
                result = runInterpreter (evalExpr prog)
            result `shouldBe` (Right (VInt 3), [])
        it "doesn't let handler use scope of effectful computation" $ do
            let prog = TryHandle (Let "x" (Int 1) (Perform Unit)) "_" (Var "x")
                result = runInterpreter (evalExpr prog)
            result `shouldBe` (Left "Unbound var: x", [])
