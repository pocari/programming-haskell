module Practice where

-- 15.9.1
-- 1 + (2*3)
-- 2*3 が最内簡約化
--
-- (1+2) * (2+3)
-- 1+2, 2+3が最内簡約化
--
-- fst (1+2, 2+3)
-- fst が最外簡約化
--
-- (\x -> 1 + x) (2*3)
-- \x -> 1 + x が最外簡約化。(xを(2*3)で置き換える)

-- 15.9.2
-- fst (1+2, 2+3)を評価する際に、最内簡約よりも最外簡約のほうが適している理由
-- 最内簡約の場合、fstの引数の、1+2, 2+3から先に評価されるため、本来不要なタブルの2ndの2+3の位置にある式が無限リストなどの場合に、評価が終了できない。
-- 最外簡約の場合、先に fstが評価されるため、その処理に必要なタブルの1ndの1+2しか評価されないため、2ndにどんな式があっても評価可能

-- 15.9.3
-- mult = \x -> (\y -> x * y)
-- に対する、 mult 3 4の評価順
-- mult 3 4
-- -> (mult 3) 4
-- -> (\y -> 3 + y) 4
-- -> 3 + 4
-- -> 7
-- の四段階で評価される

-- 15.9.4
fibs :: [Integer]
fibs = 0 : 1 : [ x + y | (x, y) <- zip fibs (tail fibs) ]

-- 15.9.5

-- 問題がよく理解できなかったので断念
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
--             deriving Show
-- 
-- t1 :: Tree Int
-- t1 = Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)
-- 
-- repeat' :: Tree a -> [Tree a]
-- repeat' Leaf = []
-- repeat' x    = x : repeat' x
-- 
-- take' :: Int -> [Tree a] -> [a]
-- take' _ Leaf         = []
-- take' 0 (Node l x r):nodexs = [x] ++ take'
-- take' n (Node l x r) = (take' (n - 1) l) ++ (take' (n - 1) r)
-- 
-- replicate' :: Int -> Tree a -> [a]
-- replicate' n = take' n . repeat'

-- 15.9.6
-- 仮の初期値
initd :: Double
initd = 1.0

-- 仮の誤差
mindelta :: Double
mindelta = 0.0000001

next' :: Double -> Double -> [Double]
next' n = iterate (\b -> (b + n / b) / 2)

sqroot :: Double -> Double
sqroot n = findSqrt
 where
  next :: Double -> Double
  next a = (a + n / a) / 2
  nexts :: [Double]
  nexts = iterate next initd
  findSqrt =
    head [ x | (x, y) <- zip nexts (tail nexts), abs (x - y) <= mindelta ]

