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

