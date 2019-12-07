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

makeN :: Int -> Bits -> Bits
makeN n bits = take n $ bits ++ repeat 0

make8 :: Bits -> Bits
make8 = makeN 8

parityBit :: Bits -> Bit
parityBit xs = if odd countBits then 1 else 0 where countBits = count 1 xs

divideParity :: Bits -> (Bits, Bit)
divideParity xs = (d, parity)
 where
  (d, [parity]) = splitAt splitPos xs
  splitPos      = length xs - 1

addParity :: Bits -> Bits
addParity xs = xs ++ [parityBit xs]

checkParity :: Bits -> Bits
checkParity xs | parityOk  = d
               | otherwise = error "invalid Bits"
 where
  parityOk    = (parityBit d) == parity
  (d, parity) = divideParity xs

chopN :: Int -> Bits -> [Bits]
chopN _ [] = []
chopN n xs = take n xs : chopN n (drop n xs)

chop8 :: Bits -> [Bits]
chop8 = chopN 8

encode :: String -> Bits
encode = addParity . concatMap (make8 . int2bin . ord)

decode :: Bits -> String
decode = map (chr . bin2int2) . chop8 . checkParity

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

