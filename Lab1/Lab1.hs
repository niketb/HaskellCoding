--My version
maxNum :: [Int] -> Int
maxNum [] = error "Cannot find maximum of empty list"
maxNum [x] = x
maxNum (x:xs) = max x (maxNum xs) 

--Version without using the max function
maxNum' :: [Int] -> Int
maxNum' [] = error "Cannot find maximum of empty list"
maxNum' [x] = x
maxNum' (x:xs) = if x > largest
				 then x
				 else largest
				 where largest = maxNum' xs

fizzbuzz :: Int -> [Char]
fizzbuzz x
 | n <= 0 = "Invalid input"
 | x == 1 = "1"  
 | x `mod` 3 == 0 && x `mod` 5 == 0 = fizzbuzz (x-1) ++ " fizzbuzz"
 | x `mod` 3 == 0 = fizzbuzz (x-1) ++ " fizz"
 | x `mod` 5 == 0 = fizzbuzz (x-1) ++ " buzz"
 | otherwise = fizzbuzz (x-1) ++ " " ++ show x

 