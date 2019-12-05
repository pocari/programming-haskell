module Sample where

-- 5.7.1
q5_7_1 :: Int
q5_7_1 = sum [ n * n | n <- [1 .. 100] ]

-- 5.7.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0 .. m], y <- [0 .. n] ]

-- 5.7.3
square :: Int -> [(Int, Int)]
square n = [ (x, y) | (x, y) <- grid n n, x /= y ]

-- 5.7.4
myReplicate :: Int -> a -> [a]
myReplicate n x = [ x | _ <- [1 .. n] ]

-- 5.7.5
pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , x * x + y * y == z * z
  ]

-- 5.7.6
perfects :: Int -> [Int]
perfects n = [ x | x <- [1 .. n], isPerfect x ]
 where
  isPerfect y = y == yakusuSum y
  yakusuSum m = sum [ z | z <- [1 .. (m - 1)], m `mod` z == 0 ]

-- 5.7.7
q5_7_7 :: [(Int, Int)]
q5_7_7 = concat [ [ (x, y) | y <- [4, 5, 6] ] | x <- [1, 2, 3] ]

-- 5.7.8
-- 問題にでてくる find 関数がどれのことかよくわからなかった

-- 5.7.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]

