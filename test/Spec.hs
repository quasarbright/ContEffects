import Test.Hspec

import Interpreter

main :: IO ()
main = hspec $ do
    describe "effect handling" $ do
        it "handles effect decorators" $ do
            let prog = TryHandle (Perform Unit) "x" (Print (Var "x") \>> Perform (Var "x"))
            let result = runEvalExpr prog
            result `shouldBe` Left "Unhandled effect: ()"
        it "handles high order effect decorators" $ do
            let prog = lets
                        [ ("decorate", "run" \->
                            TryHandle
                                (Var "run" \$ Unit)
                                "e" (Print (Var "e") \>> Perform (Var "e")))
                        ]
                        (TryHandle (Var "decorate" \$ ("_" \-> (Perform (Int 1) \+ Int 2))) "e" (Var "e"))
            -- let prog = undefined 
            let result = runInterpreter (evalExpr prog)
            result `shouldBe` (Right (VInt 3), [VInt 1]) 
