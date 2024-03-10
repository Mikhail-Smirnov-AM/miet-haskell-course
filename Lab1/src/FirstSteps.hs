module FirstSteps 
where
import Data.Word (Word8)

-- xor x y находит "исключающее или" x и y
-- xor True False == True
-- xor True True == False

-- используйте сопоставление с образцом
xor :: Bool -> Bool -> Bool
xor x y = case () of _ | (x == False && y == False) || (x == True && y == True) -> False
                     _ | otherwise -> True

-- max3 x y z находит максимум из x, y и z
-- max3 1 3 2 == 3
-- max3 5 2 5 == 5
-- median3 x y z находит второе по величине число (медиану)
-- median3 1 3 2 == 2
-- median3 5 2 5 == 5
max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = case () of _ | (z >= x) && (z >= y) -> z
                        _ | (y >= x) && (y >= z) -> y 
                        _ | otherwise -> x

median3 x y z = case () of _ | ((y <= x) && (x <= z)) || ((z <= x) && (x <= y)) -> x
                           _ | ((x <= y) && (y <= z)) || ((z <= y) && (y <= x)) -> y
                           _ | otherwise -> z

-- Типы данных, описывающие цвета в моделях 
-- RGB (https://ru.wikipedia.org/wiki/RGB), компоненты от 0 до 255
-- и CMYK (https://ru.wikipedia.org/wiki/CMYK), компоненты от 0.0 до 1.0
data RGB = RGB { red :: Word8, green :: Word8, blue :: Word8 } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)
-- Задайте функцию для их преобразования
-- (формулы из http://  www.codeproject.com/Articles/4488/XCmyk-CMYK-to-RGB-Calculator-with-source-code):
-- Black   = min(1-Red, 1-Green, 1-Blue)
-- Cyan    = (1-Red-Black) / (1-Black)
-- Magenta = (1-Green-Black) / (1-Black)
-- Yellow  = (1-Blue-Black) / (1-Black) 
-- где значения Red, Green и Blue нормализованы от 0 до 1).

-- Заметьте, что (/) для Int не работает, и неявного преобразования Int в Double нет.
-- Это преобразование производится с помощью функции fromIntegral.
rgbToCmyk :: RGB -> CMYK
rgbToCmyk color = let red_d   = fromIntegral (red color)   / (255 :: Double)
                      green_d = fromIntegral (green color) / (255 :: Double)
                      blue_d  = fromIntegral (blue color)  / (255 :: Double)
                      black_v = min (min (1 - red_d) (1 - green_d)) (1 - blue_d)
                      cyan_v = if black_v == 1.0 then 0.0 else ((1 - red_d) - black_v) / (1 - black_v)
                      magenta_v = if black_v == 1.0 then 0.0 else ((1 - green_d) - black_v) / (1 - black_v)
                      yellow_v = if black_v == 1.0 then 0.0 else ((1 - blue_d) - black_v) / (1 - black_v)    
                  in CMYK cyan_v magenta_v yellow_v black_v


                         

-- geomProgression b q n находит n-й (считая с 0) член 
-- геометрической прогрессии, нулевой член которой -- b, 
-- а знаменатель -- q.
-- geomProgression 3.0 2.0 2 == 12.0

-- используйте рекурсию
-- не забудьте случаи n < 0 и n == 0.
geomProgression :: Double -> Double -> Integer -> Double
geomProgression b q 0 = b 
geomProgression b q n = if n < 0 
                        then error "Negative n" 
                        else q*geomProgression b q (n-1)

-- coprime a b определяет, являются ли a и b взаимно простыми
-- (определение: Целые числа называются взаимно простыми, 
-- если они не имеют никаких общих делителей, кроме +/-1)
-- coprime 10 15 == False
-- coprime 12 35 == True

-- Используйте рекурсию
-- Есть ли важные пограничные случаи или вспомогательные функции? Не забудьте добавить их в тесты.

-- Полезные функции в Prelude (автоматически загруженной
-- части стандартной библиотеки): quot, rem, quotRem 
-- (или div, mod, divMod в зависимости от того, как 
-- обрабатываете отрицательные числа)
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
coprime :: Integer -> Integer -> Bool
coprime a 0 = False
coprime 0 b = False 
coprime a 1 = True
coprime 1 b = True 
coprime a (-1) = True
coprime (-1) b = True 
coprime a b = if  abs a > abs b 
              then coprime c b
              else coprime a d 
              where c = rem a b 
                    d = rem b a  
