module Main where

main :: IO ()
main = putStrLn "Hello, world"

myeven :: Integral a => a -> Bool
myeven x = x `mod` 2 == 0

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs = (take n xs, drop n xs)

myRecip :: Fractional a => a -> a
myRecip n = 1 / n

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n

mySigNum :: Int -> Int
mySigNum n = if n < 0 then -1 else if n == 0 then 0 else 1

myAbs2 :: Int -> Int
myAbs2 n | n > 0     = n
         | otherwise = -n

mySigNum2 :: Int -> Int
mySigNum2 n | n < 0     = -1
            | n == 0    = 0
            | otherwise = 1

