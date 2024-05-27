module Lab12 where

xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z | (z >= x) && (z >= y) = z
max3 x y z | (y >= x) && (y >= z) = y 
max3 x _ _ = x

data NEL a = NEL a [a] deriving (Eq, Show, Read)

listToNel :: [a] -> Maybe (NEL a)
listToNel [] = Nothing
listToNel (x:xs) = Just (NEL x xs)

nelToList :: NEL a -> [a]
nelToList (NEL x xs) = x:xs

newtype Poly a = P [a]

--x :: Num a => Poly a
--x = (P [0,1])

applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _ = 0
applyPoly (P (p:ps)) x0 = p + x0*applyPoly (P ps) x0

instance (Num a, Eq a) => Eq (Poly a) where
    P p1 == P p2 = let diff = length p2 - length p1
                       p1_ext = if diff > 0
                                then p1 ++ (replicate diff 0)
                                else p1
                       p2_ext = if diff < 0
                                then p2 ++ (replicate (abs diff) 0)
                                else p2 
                       p12_list = zipWith (-) p1_ext p2_ext
                       p12 = P p12_list
                       in and (map (\el -> (applyPoly p12 el) == 0) (p1 ++ p2))

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show (P p) = if last p /= abs (last p) -- если старший коэффициент положительный, необходимо убрать знак "+" перед ним
                 then string_concat
                 else drop 3 string_concat
                 where string_concat = concat (reverse string_nonzero_term) -- конкатенация списка одночленов
                       string_nonzero_term = filter (/= "") string_term -- удаление "пустых" одночленов из списка
                       -- объединение коэффициента с "x" и степенью одночлена
                       -- одночлены с нулевыми коэффициентами обращаются в ""
                       string_term = map (\(p_i,x_i,pow_i) -> case p_i of
                                                              0 -> ""
                                                              _ | p_i == abs p_i -> " + " ++ show p_i ++ x_i ++ pow_i
                                                              _ -> " - " ++ show (abs p_i) ++ x_i ++ pow_i) string_p_x_pow
                       string_p_x_pow = zip3 p string_x string_pow
                       -- список символов переменных со знаками возведения в степень и умножения
                       string_x = [""] ++ [" * x"] ++ (replicate (length p - 2) " * x^")
                       -- список степеней
                       string_pow = ["",""] ++ (map show [2,3..length p])

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = let diff = length p2 - length p1
                         p1_ext = if diff > 0
                                  then p1 ++ (replicate diff 0)
                                  else p1
                         p2_ext = if diff < 0
                                  then p2 ++ (replicate (abs diff) 0)
                                  else p2
                     in P (zipWith (+) p1_ext p2_ext)

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
-- умножение (a0+a1*x+...+an*x^n)*(b0+b1*x+...+bm*x^m) сводится к 
-- a0*(b0+b1*x+...+bm*x^m)+a1*x*(b0+b1*x+...+bm*x^m)
-- умножение на константу C представляет из себя умножение всех коэффициентов на C
-- умножение на x представляет собой добавление нулевого элемента в начало списка коэффициентов 
times (P [x0,x1]) (P p) = plus (P (map (x0 *) p)) (P (0:(map (x1 *) p)))
times (P (p1:p1s)) (P p2) = let term1 = P (map (p1 *) p2)
                                term2 = times (P p1s) (P p2)
                                term2x = times (P [0,1]) term2
                        in plus term1 term2x

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = ((P [-1]) *)
    fromInteger c = (P [fromInteger c])
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined