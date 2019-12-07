module Main where

main :: IO ()
main = putStrLn "hello, world"

facNoRe :: Int -> Int
facNoRe n = product [1 .. n]

facRe :: Int -> Int
facRe 0 = 1
facRe n = n * facRe (n - 1)

myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y : ys) | x <= y    = x : y : ys
                    | otherwise = y : myInsert x ys

myIsort :: [Int] -> [Int]
myIsort []       = []
myIsort (x : xs) = myInsert x (myIsort xs)

myZip :: [a] -> [b] -> [(a, b)]
myZip []       _        = []
myZip _        []       = []
myZip (x : xs) (y : ys) = [(x, y)] ++ myZip xs ys

