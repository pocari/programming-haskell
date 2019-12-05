module Main where

import           Data.Char

main :: IO ()
main = putStrLn "Hello, world"

myconcat :: [[a]] -> [a]
myconcat xss = [ x | xs <- xss, x <- xs ]

myfirsts :: [(a, b)] -> [a]
myfirsts xs = [ x | (x, _) <- xs ]

myoverN :: Ord a => a -> a -> Bool
myoverN x y = x > y

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (i, y) <- zip [0 ..] xs, y == x ]

lowers :: String -> Int
lowers str = length [ x | x <- str, 'a' <= x && x <= 'z' ]

count :: Char -> String -> Int
count c str = length [ x | x <- str, x == c ]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr $ ord 'a' + n

shift :: Int -> Char -> Char
shift n c | isLower c = int2let $ (let2int c + n) `mod` 26
          | otherwise = c

encode :: Int -> String -> String
encode n str = [ shift n x | x <- str ]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

freqs :: String -> [Float]
freqs cs = [ percent (count c cs) n | c <- ['a' .. 'z'] ] where n = lowers cs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o - e) ** 2) / e | (o, e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop (n `mod` len) xs ++ take (n `mod` len) xs
  where len = length xs

crack :: String -> String
crack xs = encode (-factor) xs
 where
  factor = head (positions (minimum chitab) chitab)
  chitab = [ chisqr (rotate n table') table | n <- [0 .. 25] ]
  table' = freqs xs

table :: [Float]
table =
  [ 8.1
  , 1.5
  , 2.8
  , 4.2
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.3
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.0
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]
