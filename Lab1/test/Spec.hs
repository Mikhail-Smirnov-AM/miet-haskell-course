import FirstSteps
import Lists
import Luhn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
        it "median3" $ do
            median3 1 2 3 `shouldBe` 2
            median3 5 4 6 `shouldBe` 5
            median3 10 35 22 `shouldBe` 22
        it "rbgToCmyk" $ do
            rgbToCmyk (RGB 0 0 0) `shouldBe` (CMYK 0.0 0.0 0.0 1.0)
            rgbToCmyk (RGB 255 0 0) `shouldBe` (CMYK 0.0 1.0 1.0 0.0)
            rgbToCmyk (RGB 0 255 0) `shouldBe` (CMYK 1.0 0.0 1.0 0.0)
            rgbToCmyk (RGB 0 0 255) `shouldBe` (CMYK 1.0 1.0 0.0 0.0)
            rgbToCmyk (RGB 255 255 0) `shouldBe` (CMYK 0.0 0.0 1.0 0.0)
            rgbToCmyk (RGB 255 0 255) `shouldBe` (CMYK 0.0 1.0 0.0 0.0)
            rgbToCmyk (RGB 0 255 255) `shouldBe` (CMYK 1.0 0.0 0.0 0.0)
            rgbToCmyk (RGB 255 255 255) `shouldBe` (CMYK 0.0 0.0 0.0 0.0)
            rgbToCmyk (RGB 100 150 200) `shouldBe` (CMYK 0.5000000000000001 0.24999999999999997 0.0 0.21568627450980393)
        it "geomProgression" $ do
            geomProgression 5.0 1.0 0 `shouldBe` 5.0
            geomProgression 5.0 0.5 1 `shouldBe` 2.5
            geomProgression 5.0 0.5 2 `shouldBe` 1.25
            geomProgression 5.0 1.5 1 `shouldBe` 7.5
            geomProgression 5.0 1.5 2 `shouldBe` 11.25
            geomProgression 0.0 1.5 10 `shouldBe` 0
            geomProgression 2.0 0.0 5 `shouldBe` 0
        it "coprime" $ do 
            coprime 2 3 `shouldBe` True
            coprime 8 9 `shouldBe` True
            coprime 10 15 `shouldBe` False
            coprime (-2) 3 `shouldBe` True
            coprime 8 (-9) `shouldBe` True
            coprime (-10) (-15) `shouldBe` False
    describe "lists" $ do
        it "distance" $ do
            distance (Point [1,2,3]) (Point [1,2,3]) `shouldBe` 0.0
            distance (Point [1,2,2]) (Point [1,2,3]) `shouldBe` 1.0
            distance (Point [1,2,3]) (Point [2,2,2]) `shouldBe` sqrt 2
        it "intersect" $ do
            intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] `shouldBe` [2, 4]
            intersect [1, 2, 4, 6] [3, 5, 7] `shouldBe` []
            intersect [] [1] `shouldBe` []
            intersect [1] [] `shouldBe` []
        it "zipN" $ do
            zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            zipN [[1, 2, 3], [4, 5], [6]] `shouldBe` [[1, 4, 6], [2, 5], [3]]
            zipN [[1], [2,3], [4,5,6]] `shouldBe` [[1, 2, 4],[3,5],[6]]
            zipN [[1, 2, 3], [4, 5], []] `shouldBe` [[1,4],[2,5],[3]]
        it "find" $ do
            find (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            find (> 0) [-1, -2, -3] `shouldBe` Nothing
            find (even) [-1, 5, -3, 4] `shouldBe` Just 4
            find (even) [-1, 3, -5] `shouldBe` Nothing
        it "findLast" $ do
            findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (> 0) [-1, -2, -3] `shouldBe` Nothing
            findLast (even) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (even) [-1, 3, -5] `shouldBe` Nothing
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
        it "tailNel" $ do
            tailNel (NEL 1 [2,3]) `shouldBe` [2,3]
            tailNel (NEL "a" ["b","c","d"]) `shouldBe` ["b","c","d"]
            tailNel (NEL 1 []) `shouldBe` []
        it "lastNel" $ do
            lastNel (NEL 1 [2,3]) `shouldBe` 3
            lastNel (NEL "a" ["b","c","d"]) `shouldBe` "d"
            lastNel (NEL 1 []) `shouldBe` 1
        it "zipNel" $ do
            zipNel (NEL 1 [2,3]) (NEL 4 [5,6]) `shouldBe` (NEL (1,4) [(2,5),(3,6)])
            zipNel (NEL 1 [2,3]) (NEL 4 [5,6,7]) `shouldBe` (NEL (1,4) [(2,5),(3,6)])
            zipNel (NEL 1 [2,3,7]) (NEL 4 [5,6]) `shouldBe` (NEL (1,4) [(2,5),(3,6)])
            zipNel (NEL 1 []) (NEL 4 []) `shouldBe` (NEL (1,4) [])
        it "listToNel" $ do 
            listToNel [1,2,3] `shouldBe` Just (NEL 1 [2,3])
            listToNel [1] `shouldBe` Just (NEL 1 [])
        it "nelToList" $ do
            nelToList (NEL 1 [2, 3]) `shouldBe` [1,2,3]
            nelToList (NEL 1 []) `shouldBe` [1]
    describe "luhn" $ do
        it "isLuhnValid" $ do
            isLuhnValid 1111222233334444 `shouldBe` True
            isLuhnValid 4561261212345467 `shouldBe` True
            isLuhnValid 1111222233334 `shouldBe` True
            isLuhnValid 4561261212345464 `shouldBe` False
            isLuhnValid 1112222233334444 `shouldBe` False
            isLuhnValid 11112222333344 `shouldBe` False
            isLuhnValid (-1) `shouldBe` False
