module Sample where

-- 6.8.1

-- 6.8.2
sumDown :: Int -> Int
sumDown 0 = 0
sumDown n = n + sumDown (n - 1)

-- 6.8.3
(^^^) :: Integer -> Integer -> Integer
_ ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n - 1))

-- 6.8.4
euclid :: Int -> Int -> Int
euclid x y | x == y    = x
           | x > y     = euclid (x - y) y
           | otherwise = euclid x (y - x)

-- 6.8.5
-- パス

-- 6.8.6-a
myAnd :: [Bool] -> Bool
myAnd []       = True
myAnd (x : xs) = x && myAnd xs

-- 6.8.6-b
myConcat :: [[a]] -> [a]
myConcat []       = []
myConcat (x : xs) = x ++ myConcat xs

-- 6.8.6-c
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- 6.8.6-d
(!!!) :: [a] -> Int -> a
(x : _ ) !!! 0 = x
(_ : xs) !!! n = xs !!! (n - 1)

-- 6.8.6-e
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys) | x == y    = True
                  | otherwise = myElem x ys

-- 6.8.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xx@(x : xs) yy@(y : ys) | x < y     = x : merge xs yy
                              | otherwise = y : merge xx ys

-- 6.8.8
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take halfIndex xs, drop halfIndex xs)
  where halfIndex = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort xx@(x : xs) | null xs   = [x]
                  | otherwise = merge (msort xs1) (msort xs2)
  where (xs1, xs2) = halve xx

-- 6.8.9-a
mySum :: Num a => [a] -> a
mySum []       = 0
mySum (x : xs) = x + mySum xs

-- 6.8.9-b
myTake :: Int -> [a] -> [a]
myTake _ []       = []
myTake 0 _        = []
myTake n (x : xs) = x : myTake (n - 1) xs

-- 6.8.9-c
myLast :: [a] -> a
myLast (x : xs) | null xs   = x
                | otherwise = myLast xs

