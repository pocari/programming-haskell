module Hoge where

bools :: [Bool]
bools = [True, False, True, False]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5, 6], [7, 8]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f = f

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f $ f x

