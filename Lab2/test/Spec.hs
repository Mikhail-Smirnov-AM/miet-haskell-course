import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ do
            applyPoly (P [1]) 1 `shouldBe` 1
            applyPoly (P [0,2,3]) 0 `shouldBe` 0
            applyPoly (P [1,2,3]) 1 `shouldBe` 6
            applyPoly (P [1,2,3]) 3 `shouldBe` 34
        it "==" $ do
           (P [1]) == (P [1]) `shouldBe` True
           (P [1]) == (P [2]) `shouldBe` False
           (P [1,2,3]) == (P [1,2,3]) `shouldBe` True 
           (P [1,2,3]) == (P [1,0,3]) `shouldBe` False
           (P [1,2,3]) == (P [1,2,3,0]) `shouldBe` True
        it "show" $ do
            show (P [0]) `shouldBe` "0"
            show (P [1]) `shouldBe` "1"
            show (P [0, 1]) `shouldBe` "1 * x"  
            show (P [0, 0, 1]) `shouldBe` "1 * x^2"
            show (P [4,3,2]) `shouldBe` "2 * x^2 + 3 * x + 4"
            show (P [0,0,-1]) `shouldBe` " - 1 * x^2"
            show (P [0,-1,1]) `shouldBe` "1 * x^2 - 1 * x"
        it "plus" $ do
            plus (P [0]) (P [0]) `shouldBe` (P [0])
            plus (P [1]) (P [2]) `shouldBe` (P [3])
            plus (P [1,2,3]) (P [4,5,6]) `shouldBe` (P [5,7,9])
            plus (P [1,2,3]) (P [4,5,6,10,11]) `shouldBe` (P [5,7,9,10,11])
            plus (P [1,2,3,10,11]) (P [4,5,6]) `shouldBe` (P [5,7,9,10,11])
        it "times" $ do 
            times (P [0]) (P [0]) `shouldBe` (P [0])
            times (P [1]) (P [2]) `shouldBe` (P [2])
            times (P [2]) (P [1,2,3]) `shouldBe` (P [2,4,6])
            times (P [0,1]) (P [1,2,3]) `shouldBe` (P [0,1,2,3])
            times (P [1,2]) (P [1,2,3]) `shouldBe` (P [1,4,7,6])
            times (P [1,2,3]) (P [1,2,3]) `shouldBe` (P [1,4,10,12,9])
        it "negate" $ do
            negate (P [1,2,3]) `shouldBe` (P [-1,-2,-3])
        it "fromInteger" $ do
            fromInteger 1 `shouldBe` (P [1])
        it "deriv" $ do
            deriv (P [0]) `shouldBe` (P [0])
            deriv (P [5]) `shouldBe` (P [0])
            deriv x `shouldBe` (P [1])
            deriv (x*x) `shouldBe` 2*x
            deriv (3*x*x+2*x+1) `shouldBe` 6*x+2
        it "nderiv" $ do
            nderiv 0 (x*x+2*x+3) `shouldBe` x*x+2*x+3
            nderiv 1 (x*x+2*x+3) `shouldBe` 2*x+2
            nderiv 2 (x*x+2*x+3) `shouldBe` P [2]
            nderiv 3 (x*x+2*x+3) `shouldBe` P [0]
    describe "simpleLang" $ do 
        it "empty" $ do
            empty "x" `shouldBe` 0
        it "extend" $ do 
            let state_extend = extend empty "x" 2
            state_extend "x" `shouldBe` 2
            state_extend "y" `shouldBe` 0
        it "eval" $ do
            let state_eval_x = extend empty "x" 2
                state_eval = extend state_eval_x "y" 5
            eval state_eval (Var "x") `shouldBe` 2
            eval state_eval (Var "y") `shouldBe` 5
            eval state_eval (Val 100) `shouldBe` 100
            eval state_eval (Op (Var "x") Plus (Var "y")) `shouldBe` 7
            eval state_eval (Op (Var "x") Minus (Var "y")) `shouldBe` (-3)
            eval state_eval (Op (Var "y") Minus (Var "x")) `shouldBe` 3
            eval state_eval (Op (Var "x") Times (Var "y")) `shouldBe` 10
            eval state_eval (Op (Var "x") Divide (Var "y")) `shouldBe` 0
            eval state_eval (Op (Var "y") Divide (Var "x")) `shouldBe` 2
            eval state_eval (Op (Var "x") Gt (Var "y")) `shouldBe` 0
            eval state_eval (Op (Var "y") Gt (Var "x")) `shouldBe` 1
            eval state_eval (Op (Var "x") Ge (Var "y")) `shouldBe` 0
            eval state_eval (Op (Var "y") Ge (Var "x")) `shouldBe` 1
            eval state_eval (Op (Var "x") Lt (Var "y")) `shouldBe` 1
            eval state_eval (Op (Var "y") Lt (Var "x")) `shouldBe` 0
            eval state_eval (Op (Var "x") Le (Var "y")) `shouldBe` 1
            eval state_eval (Op (Var "y") Le (Var "x")) `shouldBe` 0
            eval state_eval (Op (Var "x") Eql (Var "y")) `shouldBe` 0           
        it "runSimpler" $ do
            let state_x = extend empty "x" 2
                state = extend state_x "y" 5
                result_DAssign = runSimpler state (DAssign "x" (Val 4))
                result_DIf_then = runSimpler state (DIf (Op (Var "x") Lt (Var "y")) (DAssign "x" (Val 10)) (DAssign "x" (Val 20)))
                result_DIf_else = runSimpler state (DIf (Op (Var "x") Gt (Var "y")) (DAssign "x" (Val 10)) (DAssign "x" (Val 20)))
                result_DWhile = runSimpler state (DWhile (Op (Var "y") Lt (Val 8)) (DAssign "y" (Op (Var "y") Plus (Val 1))))
                result_DSequence = runSimpler state (DSequence (DAssign "x" (Val 6)) (DAssign "x" (Val 9)))
                result_DSkip = runSimpler state DSkip

            result_DAssign "x" `shouldBe` 4
            result_DIf_then "x" `shouldBe` 10
            result_DIf_else "x" `shouldBe` 20
            result_DWhile "y" `shouldBe` 8
            result_DSequence "x" `shouldBe` 9
            result_DSkip "x" `shouldBe` 2
        it "run factorial" $ do
            let state_factorial1 = extend empty "In" 1
                state_factorial2 = extend empty "In" 3
                state_factorial3 = extend empty "In" 5
                result1 = run state_factorial1 factorial
                result2 = run state_factorial2 factorial
                result3 = run state_factorial3 factorial
            result1 "Out" `shouldBe` 1
            result2 "Out" `shouldBe` 6
            result3 "Out" `shouldBe` 120
        it "run squareRoot" $ do
            let state_squareRoot1 = extend empty "A" 1
                state_squareRoot2 = extend empty "A" 16
                state_squareRoot3 = extend empty "A" 198
                result1 = run state_squareRoot1 squareRoot
                result2 = run state_squareRoot2 squareRoot
                result3 = run state_squareRoot3 squareRoot
            result1 "B" `shouldBe` 1
            result2 "B" `shouldBe` 4
            result3 "B" `shouldBe` 14
        it "run fibonacci" $ do
            let state_fibonacci1 = extend empty "In" 1
                state_fibonacci2 = extend empty "In" 5
                state_fibonacci3 = extend empty "In" 12
                result1 = run state_fibonacci1 fibonacci
                result2 = run state_fibonacci2 fibonacci
                result3 = run state_fibonacci3 fibonacci
            result1 "Out" `shouldBe` 1
            result2 "Out" `shouldBe` 8
            result3 "Out" `shouldBe` 233
            
