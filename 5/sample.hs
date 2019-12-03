module Sample where

import           Data.Char

greet :: IO ()
greet = putStrLn "Hello, world"

myconcat :: [[a]] -> [a]
myconcat xss = [ x | xs <- xss, x <- xs ]

myfirsts :: [(a, b)] -> [a]
myfirsts xs = [ x | (x, _) <- xs ]

myoverN :: Ord a => a -> a -> Bool
myoverN x y = x > y

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

