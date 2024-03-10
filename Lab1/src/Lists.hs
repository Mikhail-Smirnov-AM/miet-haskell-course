module Lists where

-- вектор задаётся списком координат
newtype Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом
distance :: Point -> Point -> Double
distance (Point []) (Point []) = 0
distance x (Point []) = error "Different length"
distance (Point []) y = error "Different length"
distance (Point (x0 : xs)) (Point (y0 : ys)) = sqrt ((x0-y0)**2 + (distance (Point xs) (Point ys))**2)

-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
intersect :: [Integer] -> [Integer] -> [Integer]
intersect xs [] = []
intersect [] ys = []
intersect (x:xs) ys = if elem x ys then x:(intersect xs ys) else intersect xs ys

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
-- zipN [[1, 2, 3], [4, 5], [6]] == [[1, 4, 6], [2, 5], [3]]
zipN :: [[a]] -> [[a]]
zipN [] = []
zipN ([]:xss) = zipN xss
zipN ((x:xs) : xss) = (x:[h | (h:_) <- xss]):(zipN(xs:[t | (_:t) <- xss]))

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) = if f x then Just x else find f xs

find_filter :: (a -> Bool) -> [a] -> Maybe a
find_filter f xs = if null filter_x 
                   then Nothing
                   else Just (filter_x !! 0)
                   where filter_x = filter f xs

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast f xs = if null filter_x 
                then Nothing
                else Just (filter_x !! ((length filter_x) - 1))
                where filter_x = filter f xs

-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]
mapFuncs [] x = []
mapFuncs (f:fs) x = (f x):(mapFuncs fs x)

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds
-- и возвращает True, если все они выполняются (т.е. возвращают True) для x.
-- Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?)
satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll [] _ = True
satisfiesAll (pred0:preds) x = if pred0 x then (satisfiesAll preds x) else False


-- Непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
-- Например, NEL 1 [2, 3] соотвествует списку [1, 2, 3], а NEL 1 [] -- списку [1].
data NEL a = NEL a [a] deriving (Eq, Show, Read)

-- Запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов
-- без вызовов error) и реализуйте функции на NEL, аналогичные tail, last и zip
tailNel :: NEL a -> [a]
tailNel (NEL x xs) = xs

lastNel :: NEL a -> a
lastNel (NEL x []) = x
lastNel (NEL x (x0:xs)) = lastNel(NEL x0 xs)

zipNel :: NEL a -> NEL b -> NEL (a,b)
zipNel x y = listToNel (zip (nelToList x) (nelToList y))

listToNel :: [a] -> NEL a
listToNel [] = error "Cannot convert empty list to NEL"
listToNel (x:xs) = NEL x xs

nelToList :: NEL a -> [a]
nelToList (NEL x xs) = x:xs
