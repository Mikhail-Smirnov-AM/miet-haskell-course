import FunctorsMonads
import Streams hiding (main)
import Lab12
import Data.Either
import Test.Hspec
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Proxy
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
--import Test.Hspec.Hedgehog

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).

instance Arbitrary a => Arbitrary (NEL a) where
    arbitrary = do 
                x <- arbitrary
                xs <- arbitrary
                return (NEL x xs)

instance Arbitrary a => Arbitrary (Poly a) where
    arbitrary = do 
                xs <- arbitrary
                return (P xs)

instance Arbitrary a => Arbitrary (Stream a) where
    arbitrary = do 
                x <- arbitrary
                xs <- arbitrary
                return (x:>xs)


main :: IO ()
main = hspec $ do
    describe "property testing lab1-lab2" $ do
        prop "xor" $ (\x y -> xor x y === ((x && not y) || (not x && y)))
        
        let prop_max3 :: Integer -> Integer -> Integer -> Bool
            prop_max3 x y z = let m = max3 x y z
                              in (m >= x) && (m >= y) && (m >= z)
        prop "max3" $ prop_max3

        prop "NEL_to_list" $ (\xs -> Just (xs::(NEL Int)) === listToNel (nelToList xs))

        prop "Poly + commutativity" $ (\xs ys -> (xs::Poly Int) + (ys::Poly Int) === ys + xs) 
        prop "Poly + associativity" $ (\xs ys zs -> (xs::Poly Int) + ((ys::Poly Int) + (zs:: Poly Int)) === (xs+ys)+zs) 
        prop "Poly zero element" $ (\xs -> (xs::Poly Int) + fromInteger 0 === xs)
        prop "Poly negate" $ (\xs -> (xs::Poly Int) + (-xs) === fromInteger 0)
        prop "Poly * commutativity" $ (\xs ys -> (xs::Poly Int) * (ys::Poly Int) === ys * xs) 
        prop "Poly * associativity" $ (\xs ys zs -> (xs::Poly Int) * ((ys::Poly Int) * (zs:: Poly Int)) === (xs*ys)*zs) 
        prop "Poly identity element" $ (\xs -> (xs::Poly Int)*(fromInteger 1) === xs)
        prop "Poly distributivity" $ (\xs ys zs -> ((xs::Poly Int) + (ys::Poly Int)) * (zs:: Poly Int) ===  xs*zs + ys*zs )

    describe "functors and monads" $ do
        it "liftA2'" $ do
            liftA2' (+) (Just 1) (Just 2) `shouldBe` Just 3
            liftA2' (+) Nothing (Just 2) `shouldBe` Nothing
            
            liftA2' (+) [1,2] [4,5] `shouldBe` [5,6,6,7]

            let liftA2_right :: Int -> Int -> Either Int Int
                liftA2_right x y = liftA2' (*) (Right x) (Right y)
            liftA2_right 4 5 `shouldBe` Right 20
            
            liftA2' (-) (Left 4) (Right 5) `shouldBe` (Left 4)

            (liftA2' (+) (\x -> 2*x) (\y -> 3*y)) 3 `shouldBe` (2*3+3*3) 
            
        it "seqA" $ do
            seqA [Just 1, Just 2] `shouldBe` Just [1, 2]
            seqA [Just 1, Just 2, Nothing] `shouldBe` Nothing

            seqA [[1,2,3],[4,5,6]] `shouldBe` [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
        
            --seqA [Right 3, Right 10] `shouldBe` (Right [3,10])
            seqA [Left 3, Right 10] `shouldBe` (Left 3)

            (seqA [(\x-> even x), (\y -> not $ even y)]) 10 `shouldBe` [True,False]
            (seqA [(\x-> 3*x), (\y -> y - 5)]) 10 `shouldBe` [30,5]
            

        it "traverseA" $ do
            traverseA Just [1, 2] `shouldBe` Just [1, 2]
            traverseA (\a -> if a > 2 then Just a else Nothing) [1, 3] `shouldBe` Nothing
            
            traverseA (\a -> [a]) [1,2,3] `shouldBe` [[1,2,3]]
            traverseA (\a -> [a,2*a]) [1,2] `shouldBe` [[1,2],[1,4],[2,2],[2,4]]

            traverseA (\a -> if a > 2 then Right a else Left 0) [1,2,3,4] `shouldBe` (Left 0)
            traverseA (\a -> if a > 0 then Right a else Left 0) [1,2,3,4] `shouldBe` (Right [1,2,3,4])
            
            (traverseA (\x -> const x) [1,2,3]) 1 `shouldBe` (const [1,2,3]) 1
            (traverseA (\x -> (\y -> x*y)) [1,2,3]) 5 `shouldBe` (\y -> [y,2*y,3*y]) 5
        
        it "filterA" $ do
            filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] `shouldBe` Just [1, 2]
            filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] `shouldBe` Nothing
            
            filterA (\a -> if a > 0 then [even a] else [False]) [-1,-2,1,2] `shouldBe` [[2]]
            
            filterA (\a -> if even a then Right (a > 5) else Left False) [2, 4, 6, 8] `shouldBe` Right [6,8]
            filterA (\a -> if even a then Right (a > 5) else Left False) [1, 2, 3, 4] `shouldBe` Left False

            ((filterA (\a -> if even a then (\_ -> a > 5) else (\_ -> a < 5)) [2,4,6,8]) 1) `shouldBe` ((\x->[6,8]) 1)
            ((filterA (\a -> if even a then (\_ -> a > 5) else (\_ -> a < 5)) [1,2,6,7]) 1) `shouldBe` ((\x->[1,6]) 1)
            
    describe "streams" $ do
        let prop_sRepeat :: Int -> Int -> Int -> Property
            prop_sRepeat x i j = let s = streamToList $ sRepeat x
                                     s_i = s !! (abs i)
                                     s_j = s !! (abs j) 
                                 in x === s_i .&&. s_i === s_j
        prop "sRepeat" $ prop_sRepeat

        let prop_sCycle :: [Int] -> Int -> Property
            prop_sCycle xs i = let s = streamToList $ sCycle xs
                                   n = length xs
                                   i_a = abs i
                                   s_i = s !! i_a
                                   s_in = s !! (i_a + n)
                                   i_mod_n = mod i_a n
                                   xs_i = xs !! i_mod_n 
                               in not (null xs) ==> xs_i === s_i .&&. s_i === s_in
        prop "sCycle" $ prop_sCycle

        let prop_sIterate :: Fun Int Int -> Int -> Int -> Property
            prop_sIterate (Fn f) x i = let s = streamToList $ sIterate f x
                                           s_i = s !! (abs i)
                                           s_i1 = s !! (abs i + 1)
                                       in s_i1 === f s_i
        prop "sIterate" $ prop_sIterate

        let prop_sInterleave :: Stream Int -> Stream Int -> Int -> Property
            prop_sInterleave s1 s2 i = let s1_l = streamToList s1
                                           s2_l = streamToList s2
                                           s_l = streamToList $ sInterleave s1 s2 
                                           i_a = abs i
                                           s1_i = if even i_a --(mod i_a 2) == 0
                                                  then s1_l !! (div i_a 2)
                                                  else s2_l !! (div i_a 2)
                                           s2_i = if even i_a --(mod i_a 2) == 0
                                                  then s2_l !! (div i_a 2)
                                                  else s1_l !! (div (i_a+1) 2)
                                           s_i = s_l !! i_a
                                           s_i1 = s_l !! (i_a+1)
                                       in s1_i === s_i .&&. s2_i === s_i1
        prop "sInterleave" $ prop_sInterleave

        let prop_nats :: Int -> Property
            prop_nats i = let s = streamToList nats
                              s_i = s !! (abs i)
                              s_i1 = s !! ((abs i) + 1)
                          in s_i + 1 === s_i1
        prop "nats" $ prop_nats

        let prop_ruler :: Int -> Property
            prop_ruler i = let s = streamToList ruler
                               s_i = s !! (abs i)
                               s_2i = s !! (((abs i)+1)*2-1)
                           in if even (abs i)
                              then s_i === 0
                              else (s_i + 1) === s_2i 
                              
        prop "ruler" $ prop_ruler

        let prop_minMax :: [Int] -> Bool
            prop_minMax xs = case minMaxBang xs of
                                  Nothing -> null(xs)
                                  Just (min_xs,max_xs) -> all (\x -> x >= min_xs) xs && all (\x -> x <= max_xs) xs
        prop "minMax" $ prop_minMax

        let prop_minMax_eq :: [Int] -> Property
            prop_minMax_eq xs = let res1 = minMaxSlow xs
                                    res2 = minMax xs
                                    res3 = minMaxBang xs
                                in res1 === res2 .&. res2 === res3
        prop "minMax equality" $ prop_minMax_eq


    -- необходимо, чтобы сравнение для stream вычислялось не бесконечно
    --describe "streams laws" $ do
    --    it "functorLaws" $ do
    --        lawsCheck (functorLaws (Proxy :: Proxy Stream))
    --    it "applcativeLaws" $ do
    --        lawsCheck (applicativeLaws (Proxy :: Proxy Stream))
    --    it "monadLaws" $ do
    --        lawsCheck (monadLaws (Proxy :: Proxy Stream))
    --    it "foldableLaws" $ do
    --        lawsCheck (foldableLaws (Proxy :: Proxy Stream))
    --    it "traversableLaws" $ do
    --        lawsCheck (traversableLaws (Proxy :: Proxy Stream))
            
        