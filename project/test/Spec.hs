import Lib
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
    describe "test problems" $ do
        it "f(u) = -2(u-1)/(u^2+1) u(0)=u(3pi)=0" $ do --max_du0 = 0.5
            let sol = shooting_method (\u -> -2*(u-1)/(u^2+1)) (0,3*pi) 1e-2 0.5 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = sinh(u)/2-u u(0)=u(2pi)=0" $ do -- max_du0 = 1.5
            let sol = shooting_method (\u -> (sinh u)/2-u) (0,2*pi) 1e-2 1.1 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = -2u/(1+u^2)^2 u(0)=u(2pi)=0" $ do -- max_du0 = 1
            let sol = shooting_method (\u -> -2*u/(1+u^2)^2) (0,2*pi) 1e-2 1 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = 2/(1+u^2)-u^2 u(0)=u(pi)=0" $ do --max_du0 = 1.5
            let sol = shooting_method (\u -> 2/(1+u^2)-u^2) (0,pi) 1e-2 1.5 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = -(1-u^2)/(1+u^2) u(0)=u(2pi)=0" $ do --max_du0 = 1.5
            let sol = shooting_method (\u -> -(1-u^2)/(1+u^2)) (0,2*pi) 1e-2 1.5 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = -u(u-1)e^u u(0)=u(pi)=0" $ do --max_du0 = 1.5
            let sol = shooting_method (\u -> -u*(u-1)*(exp u)) (0,pi) 1e-2 1.5 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = -(u^2-1)e^(-u) u(0)=u(3pi)=0" $ do --max_du0 = 1
            let sol = shooting_method (\u -> -(u^2-1)*(exp (-u))) (0,3*pi) 1e-2 1 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 

        it "f(u) = u^2-u^3 u(0)=u(2pi)=0" $ do -- max_du0 = 2
            let sol = shooting_method (\u -> u^2-u^3) (0,2*pi) 1e-2 2 0 1e-6
            abs ((fst $ last sol) - (fst $ head sol)) `shouldSatisfy` (< 1e-3) 


