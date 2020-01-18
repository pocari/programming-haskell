module Sample where

reverse1 :: [a] -> [a]
reverse1 []       = []
reverse1 (x : xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 xs = reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' []       ys = ys
reverse' (x : xs) ys = reverse' xs (x : ys)

data Tree a = Leaf a | Node (Tree a) (Tree a)
          deriving Show

t1 :: Tree Int
t1 = Node (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) (Leaf 5)

flatten1 :: Tree a -> [a]
flatten1 (Leaf x  ) = [x]
flatten1 (Node l r) = flatten1 l ++ flatten1 r

flatten2 :: Tree a -> [a]
flatten2 t = flatten' t []

flatten' :: Tree a -> [a] -> [a]
flatten' (Leaf x  ) ys = x : ys
flatten' (Node l r) ys = flatten' l (flatten' r ys)

data Expr = Val Int | Add Expr Expr
          deriving Show

e1 :: Expr
e1 = Add (Add (Add (Val 1) (Val 2)) (Val 3)) (Val 4)

eval :: Expr -> Int
eval (Val n  ) = n
eval (Add l r) = eval l + eval r

-- *Sample> e1
-- Add (Add (Add (Val 1) (Val 2)) (Val 3)) (Val 4)
-- *Sample> eval e1
-- 10

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
        deriving Show

exec :: Code -> Stack -> Stack
exec []           s           = s
exec (PUSH n : c) s           = exec c (n : s)
exec (ADD    : c) (x : y : s) = exec c ((x + y) : s)
exec _            _           = error "invalid pattern"

comp1 :: Expr -> Code
comp1 (Val n  ) = [PUSH n]
comp1 (Add l r) = comp1 l ++ comp1 r ++ [ADD]

comp2 :: Expr -> Code
comp2 e = comp' e []

comp' :: Expr -> Code -> Code
comp' (Val n  ) c = PUSH n : c
comp' (Add l r) c = comp' l (comp' r (ADD : c))

-- *Sample> e1
-- Add (Add (Add (Val 1) (Val 2)) (Val 3)) (Val 4)
-- *Sample> comp e1
-- [PUSH 1,PUSH 2,ADD,PUSH 3,ADD,PUSH 4,ADD]
-- *Sample> exec (comp e1) []
-- [10]

data Nat = Zero | Succ Nat
         deriving Show

n1 :: Nat
n1 = Succ (Succ (Succ Zero))

n2 :: Nat
n2 = Succ (Succ Zero)

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

-- 16.9.1
-- add n (Succ m) = Succ (add n m)
--
-- 基底部:
-- add Zero (Succ m)
-- = {addを適用)
-- Succ m
-- = {mに関してaddを逆適用}
-- Succ (add Zero m)
--
-- 再帰部: Succ (add (Succ x) m) を導出する
-- add (Succ x) (Succ m)
-- = {addを適用}
-- Succ (add x (Succ m))
-- = {xに関して仮定を適用}
-- Succ (Succ (add x m))
-- = {addを逆適用}
-- Succ (Succ (add Zero (add x m)))
-- = {add x m = z  とする}
-- Succ (Succ (add Zero z))
-- = {内側のsuccに関して基底部を逆適用する}
-- Succ (add Zero (Succ z))
-- = {addを適用}
-- Succ (Succ z)
-- = {zを展開}
-- Succ (Succ (add x m))
-- = {addを逆適用}
-- Succ (add (Succ n) m)
-- 終わり

-- 16.9.2
-- 定理1: add n (Succ m) = Succ (add n m) 16.9.1の内容
-- 定理2: add n Zero = n
-- は使っていいとして
--
-- add n m = add m n
--
-- を証明する
--
-- 基底部: add Zero m から add m Zero を導出する
-- add Zero m
-- = {addを適用}
-- m
-- = {定理2を逆適用}
-- add m Zero
--
-- 再帰部: add (Succ n) m から add m (Succ n) を導出する
-- add (Succ n) m
-- = {addを適用}
-- Succ (add n m)
-- = {仮定を適用}
-- Succ (add m n)
-- = {addを逆適用}
-- add (Succ m) n
-- 終わり
-- 定理1使ってないけどいいのか？？

-- 16.9.3
-- all (== x) (replicate n x)
--
-- n = 0のとき
-- all (== x) (replicate 0 x)
-- = replicate を計算
-- all (== x) []
-- = allを計算
-- True
--
-- n = k で成り立つとして、n = k + 1の場合
-- all (== x) (replicate (k + 1) x)
-- = replicateを計算
-- all (== x) (x : replicate k x)
-- = all を適用
-- ((== x) x) && all (== x) (replicate k x)
-- = &&の左辺を計算
-- True && all (== x) (replicate k x)
-- = &&の右辺に仮定を適用
-- True && True
-- = && を計算
-- True

-- 16.9.4
-- 定理1: []     ++ ys = ys
-- 定理2: (x:xs) ++ ys = x : (xs ++ ys)
-- として
--
-- 16.9.4.1
-- xs ++ [] = xs
--
-- 基底部:
-- [] ++ []
-- = {++を計算}
-- []
--
-- 再帰部:
-- (x:xs) ++ []
-- = {定理2を適用}
-- x : (xs ++ [])
-- = {仮定を適用}
-- x : xs
--
-- 16.9.4.2
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
--
-- 基底部:
-- [] ++ (ys ++ zs)
-- = {定理1を適用}
-- ys ++ zs
-- = {ysに定理1を逆適用}
-- ys ++ zs
-- = ([] ++ ys) ++ zs
--
-- 再帰部:
-- (x:xs) ++ (ys ++ zs)
-- = {定理2を適用}
-- x : (xs ++ (ys ++ zs))
-- = 仮定を適用
-- x : ((xs ++ ys) ++ zs)
-- = 定理2を逆適用
-- (x : (xs ++ ys)) ++ zs
-- = 定理2を逆適用
-- ((x:xs) ++ ys) ++ zs
-- 終わり

-- 16.9.5
-- take n xs ++ drop n xs = xs
--
-- n = 0, xs = []の場合
-- take 0 xs ++ drop 0 xs
-- = {takeと、dropをそれぞれ計算}
-- [] ++ []
-- = {++を計算}
-- []
--
-- n > 1で再帰部
-- take (k + 1) (x:xs) ++ drop (k + 1) (x:xs)
-- = {take、dropをそれぞれ計算}
-- (x : take k xs) ++ (drop k xs)
-- = {16.9.4の(x:xs) ++ ys = x : (xs ++ ys)より}
-- x : (take k xs ++ drop k xs)
-- = {仮定より}
-- x : xs
-- 終わり
