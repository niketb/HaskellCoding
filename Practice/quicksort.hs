quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort (filter (<=x) xs)) ++ [x] ++ (quicksort (filter (>x) xs))

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
 let smallerOrEqual = [a | a <- xs, a <= x]
     larger = [a | a <- xs, a > x]
 in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger 

-- cylinder :: Double - Double -> Double
-- cylinder r h = 
-- 	let sideArea = 2 * pi * r * h
-- 		topArea = pi * r ^ 2
-- 	in sideArea + 2 * topArea

-- collatz :: Int -> [Int]
-- collatz x = collatz' x ([] ++ [x])

-- collatz' :: Int -> [Int] -> [Int]
-- collatz' 1 acc = acc
-- collatz' x acc
--  | x <= 0 = error "Non-positive numbers not allowed"
--  | (x `mod` 2) == 0 = collatz' (x `div` 2) (acc ++ [x `div` 2]) 
--  | (x `mod` 2) == 1 = collatz' (x * 3 + 1) (acc ++ [x * 3 + 1])
--  | otherwise = error "The impossible has occurred"

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
 | even n = n:collatz(n `div` 2)
 | odd n = n:collatz(n * 3 + 1)

-- Configure sublime text to set tabs to spaces

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x):acc) [] xs

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

foldElem :: (Eq a) => a -> [a] -> Bool
foldElem x lst = foldl (\acc y -> acc || (x == y)) False lst

foldMax :: (Ord a) => [a] -> a
foldMax lst = foldl1 (\acc x -> if (x > acc) then x else acc) lst

foldReverse :: [a] -> [a]
foldReverse xs = foldr (\x acc -> acc ++ [x]) [] xs
