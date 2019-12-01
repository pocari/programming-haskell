module Chap4 where

greet :: IO ()
greet = putStrLn "Hello, world"

-- 1
halve :: [a] -> ([a], [a])
halve xs | even $ length xs = splitAt (length xs `div` 2) xs
         | otherwise        = (xs, [])

-- 2
third1 :: [a] -> a
third1 xs = head $ tail $ tail xs

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_ : _ : x : _) = x

-- 3
safetail1 :: [a] -> [a]
safetail1 xs = if length xs == 0 then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | length xs == 0 = []
             | otherwise      = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- 4
(|||) :: Bool -> Bool -> Bool
True  ||| _ = True
False ||| a = a

-- 5

(&&&&) :: Bool -> Bool -> Bool
(&&&&) x y = if x then if y then True else False else False

-- 6
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False

-- 7
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

multLambda :: Int -> Int -> Int -> Int
multLambda = \x -> (\y -> (\z -> x * y * z))

-- 8
luhnDouble :: Int -> Int
luhnDouble n | n2 > 9    = n2 - 9
             | otherwise = n2
  where n2 = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
  where total = luhnDouble a + b + luhnDouble c + d
