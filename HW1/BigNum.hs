{-
  Name: Niket Bhodia
  Class: CS 252
  Assigment: HW1
  Date: 09/04/17
  Description: Implements a simplified BigNum module
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = reverse $ stripLeadingZeroes $ reverse $ bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] c = [c]
bigAdd' [] y c = bigAdd' [0] y c
bigAdd' x [] c = bigAdd' x [0] c
bigAdd' (x:xs) (y:ys) c
  | sum > 999 = (sum - 1000):(bigAdd' xs ys 1)
  | otherwise = sum:(bigAdd' xs ys 0)
  where sum = x + y + c

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] 1 = error "Negative numbers not supported"
bigSubtract' [] [] 0 = [0]
bigSubtract' [] y c = error "Negative numbers not supported"
bigSubtract' x [] c = bigSubtract' x [0] c
bigSubtract' (x:xs) (y:ys) c
  | (x - c) < y = (x - c + 1000 - y):(bigSubtract' xs ys 1)
  | otherwise = (x - c - y):(bigSubtract' xs ys 0)

bigEq :: BigNum -> BigNum -> Bool
bigEq [] [] = True
bigEq [] [y] = False
bigEq [x] [] = False
bigEq (x:xs) (y:ys) = (x==y) && (bigEq xs ys)

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [0] [0] = [0]
bigMultiply x [0] = [0]
bigMultiply [0] y = [0]
bigMultiply x y = bigAdd x $ bigMultiply x $ bigSubtract y [1] 

-- bigReplicate :: BigNum -> BigNum -> [BigNum]
-- bigReplicate [0] y = y:[]
-- bigReplicate x y = y:(bigReplicate (bigSubtract x [1]) y) 

-- bigListSum :: [BigNum] -> BigNum
-- bigListSum lst = bigListSum' [0] lst     

-- bigListSum' :: BigNum -> [BigNum] -> BigNum
-- bigListSum' x [] = x
-- bigListSum' x (y:ys) = bigListSum' (bigAdd x y) ys

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf x [0] = [1]
bigPowerOf x y = bigMultiply x $ bigPowerOf x $ bigSubtract y [1]

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]