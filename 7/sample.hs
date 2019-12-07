
module Sample where

import           Data.Char
import           Data.List

greet :: IO ()
greet = putStrLn "Hello, World"

-- bit encode decode
type Bit = Int
type Bits = [Bit]

bin2int :: Bits -> Int
bin2int bits = sum [ w * b | (w, b) <- zip weights bits ]
  where weights = iterate (* 2) 1

bin2int2 :: Bits -> Int
bin2int2 = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> Bits
int2bin 0 = []
int2bin n = m : int2bin d where (d, m) = n `divMod` 2

make8 :: Bits -> Bits
make8 bits = take 8 $ bits ++ repeat 0

chop8 :: Bits -> [Bits]
chop8 [] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)

encode :: String -> Bits
encode = concatMap (make8 . int2bin . ord)

decode :: Bits -> String
decode = map (chr . bin2int2) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: Bits -> Bits
channel = id

-- vote
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = sort [ (count x xs, x) | x <- rmdups xs ]


winner :: Ord a => [a] -> a
winner = snd . last . result

-- vote 2
ballots :: [[String]]
ballots =
  [ ["Red", "Green"]
  , ["Blue"]
  , ["Green", "Red", "Blue"]
  , ["Blue", "Green", "Red"]
  , ["Green"]
  ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank xs = map snd $ result $ map head xs

winner' :: Ord a => [[a]] -> a
winner' xs = case rank (rmempty xs) of
  [c    ] -> c
  (c : _) -> winner' (elim c xs)

