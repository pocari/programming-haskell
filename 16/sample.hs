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

data Nat = Zero | Succ Nat deriving Show

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
-- = {定理1を適用}
-- add m (Succ n)

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

-- 16.9.6
data Tree2 = Leaf2 Int | Node2 Tree2 Tree2

t21 :: Tree2
t21 = Node2 (Node2 (Node2 (Leaf2 1) (Node2 (Leaf2 2) (Leaf2 3))) (Leaf2 4))
            (Leaf2 5)

countLeaf :: Tree2 -> Int
countLeaf (Leaf2 _  ) = 1
countLeaf (Node2 l r) = countLeaf l + countLeaf r

countNode :: Tree2 -> Int
countNode (Leaf2 _  ) = 0
countNode (Node2 l r) = countNode l + countNode r + 1

-- 証明したいこと
-- countLeaf t = countNode t + 1
--
-- 基底部:
-- countLeaf (Leaf2 n)
-- = countLeafを適用
-- 1
--
-- countNode (Leaf2 n) + 1
-- = {countLeafを適用}
-- 0 + 1
-- = {+ を計算}
-- 1
--
-- 再帰部:
-- countLeaf (Node l r)
-- = {countLeafを適用}
-- countLeaf l + countLeaf r
-- = {l, rに関する仮定より}
-- (countNode l + 1) + (countNode r + 1)
-- = countNode l + countNode r + 2
--
-- countNode (Node l r) + 1
-- = {countNodeを適用}
-- (countNode l + countNode r + 1) + 1
-- = {計算}
-- countNode l + countNode r + 2

-- 16.9.7
-- functor則
-- fmap id = id
-- fmap (g . h) = fmap g . fmap h

-- 1つ目
-- fmap id Nothing
-- = {fmap適用}
-- Nothing
--
-- id Nothing
-- = {idを適用}
-- Nothing
--
-- fmap id (Just 5)
-- = {fmap適用}
-- Just 5
--
-- id (Just 5)
-- = {idを適用}
-- = Just 5
--
-- 2つ目
-- fmap (g . h) Nothing
-- = {fmap適用}
-- Nothing
--
-- (fmap g . fmap h) Nothing
-- = {(f . g) x = f (g x) を適用}
-- fmap g (fmap h Nothing)
-- = {内側のfmapを適用}
-- fmap g Nothing
-- = {fmapを適用}
-- Nothing
--
-- fmap (g . h) (Just 5)
-- = {fmapを適用}
-- Just ((g . h) 5)
-- = {(f . g) x = f (g x) を適用}
-- Just (g (h 5))
-- 
-- (fmap g . fmap h) (Just 5)
-- = {(f . g) x = f (g x) を適用}
-- fmap g (fmap h (Just 5))
-- = {内側のfmapを適用}
-- fmap g (Just (h 5))
-- -- {fmap適用}
-- Just (g (h 5))

-- 16.9.8
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x  ) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- # functor則
-- ## fmap id = id
--
-- 基底部: fmap id (Leaf x) から id (Leaf x) を導出
-- fmap id (Leaf x)
-- = {fmapを適用}
-- Leaf (id x)
-- = {idを適用}
-- Leaf x
-- = {idを逆適用}
-- id (Leaf x)
--
-- 再帰部: fmap id (Node l r) から id (Node l r)を導出
-- fmap id (Node l r)
-- = {fmapを適用}
-- Node (fmap id l) (fmap id r)
-- = {l, rに関してfmap適用}
-- Node l r
-- = {idを逆適用}
-- id (Node l r)
-- 終わり
--
-- ## fmap (g . h) = fmap g . fmap h
--
-- 基底部: fmap (g . h) (Leaf x) から (fmap g . fmap h) (Leaf x) を導出
-- fmap (g . h) (Leaf x)
-- = {fmap を適用}
-- Leaf ((g . h) x)
-- = {. を適用}
-- Leaf (g (h x))
-- = {gに関してfmapを逆適用}
-- fmap g (Leaf (h x))
-- = {fに関してfmapを逆適用}
-- fmap g (fmap h (Leaf x))
-- = . を逆適用
-- (fmap g . fmap h) (Leaf x)
-- 
-- 再帰部: fmap (g . h) (Node l r) から (fmap g . fmap h) (Node l r) を導出
-- fmap (g . h) (Node l r)
-- = {fmapを適用}
-- Node (fmap (g . h) l) (fmap  (g . h) r)
-- = {仮定を適用}
-- Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
-- = {.を適用}
-- Node (fmap g (fmap h l)) (fmap g (fmap h r))
-- = {fmapを逆適用}
-- fmap g (Node (fmap h l) (fmap h r))
-- = {fmapを逆適用}
-- fmap g (fmap h (Node h l))
-- = {.を逆適用}
-- (fmap g . fmap h) (Node h l)
-- 終わり
--
-- 16.9.9
-- Applicative則
--   pure id <*> x = x
--   pure (g x) = pure g <*> pure x
--   x <*> pure y = pure (\g -> g y) <*> x
--   x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
--
-- # pure id <*> x = x
-- ## Nothingの場合
-- pure id <*> Nothing
-- = <*> を計算
-- Nothing
--
-- ## Just xの場合
-- pure id <*> (Just x)
-- = {<*>を計算}
-- Just (id x)
-- = {idを適用}
-- Just x
--
-- # pure (g x) = pure g <*> pure x
-- ## Nothingの場合
-- pure (g Nothing)
-- = {pureを適用}
-- Just (g Nothing)
-- = {fmapを逆適用}
-- fmap g (Just Nothing)
-- = Myabeのfmap = <*>なので置換
-- (Just g) <*> (Just Nothing)
-- = {それぞれのJustにpureを逆適用}
-- pure g <*> pure Nothing
--
-- ## Just xの場合 pure (g (Just x)) から pure g <*> pure (Just x)
-- pure (g (Just x))
-- = {pureを適用}
-- Just (g (Just x))
-- = {fmapを逆適用}
-- fmap g (Just (Just x))
-- = {Myabeのfmap = <*>なので置換}
-- (Just g) <*> (Just (Just x))
-- = {それぞれのJustにpureを逆適用}
-- pure g <*> pure (Just x)

-- # x <*> pure y = pure (\g -> g y) <*> x
-- ## Nothingの場合 Nothing <*> pure y から pure (\g -> g y) <*> Nothing を導出
-- Nothing <*> pure y
-- = {<*> を適用}
-- Nothing
--
-- pure (\g -> g y) <*> Nothing
-- = {<*>を適用}
-- Nothing
--
-- ## Just xの場合 (Just x) <*> pure y から pure (\g -> g y) <*> (Just x) を導出
-- (Just x) <*> pure y
-- = {<*> を適用}
-- Just (x y)
-- = {x を (\g -> g y) に置き換え}
-- Just ((\g -> g y) x)
-- = {<*>を逆適用}
-- pure (\g -> g y) <*> (Just x)
--
-- # x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- ## Nothingの場合: Nothing <*> (y <*> z) から (pure (.) <*> Nothing <*> y) <*> z を導出
-- Nothing <*> (y <*> z)
-- = {外側の<*>を適用}
-- Nothing
-- = {<*>を逆適用}
-- Nothing <*> z
-- = {<*>を逆適用}
-- (Nothing <*> y) <*> z
-- = {<*>を逆適用}
-- (pure (.) <*> Nothing <*> y) <*> z
--
-- ## Just xの場合: (Just x) <*> (y <*> z) から (pure (.) <*> (Just x) <*> y) <*> z を導出
-- こっちがどうもわからず。
--
-- 16.9.10
-- 使う定理
-- 定理1
-- [k | k <- xs] = xs
--
-- 基底部: [k | k <- []] から []を導出
-- [k | k <- []]
-- = {定義から}
-- []
--
-- 再帰部: [k | k <- (x:xs)] から (x:xs) を導出
-- [k | k <- (x:xs)]
-- = {定義から}
-- x : [k | k <- xs]
-- = {仮定から}
-- x : xs
-- 終わり
--
--
-- instance Monad [] where
--   -- (>>=) :: [a] -> (a -> [b]) -> [b]
--   xs >>= f [y | x <- xs, y <- f x]
--
-- Monad則
--   return x >>= f   = f x
--   mx >>= return    = mx
--   (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
--
-- # return x >>= f = f x
-- return x >>= f
-- = {return を適用}
-- [x] >>= f
-- = {>>=を適用}
-- [y | k <- [x], y <- f k]
-- = {外側のkはxのみなので}
-- [y | y <- f x]
-- = {定理1から}
-- f x
-- 終わり
-- 
-- # mx >>= return    = mx
-- [m | n <- mx, m <- return n]
-- = {return nを適用}
-- [m | n <- mx, m <- [n]]
-- = {m <- [n] = nから}
-- [n | n <- mx]
-- = {定理1から}
-- mx
-- 終わり
--
-- # (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
--
-- mx >>= f
-- [n | m <- mx, n <- f m]
-- concat $ map f mx
--
-- # (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
-- (concat $ map f mx) >>= g
-- concat $ map g (concat $ map f mx)
--
-- mx >>= (\x -> (f x >>= g))
-- concat $ map (\x -> (f x >>= g)) mx
-- concat $ map (\x -> (concat $ map g (f x))) mx
-- これも断念
--
-- 16.9.11
-- comp' e c = comp e ++ c
--
comp :: Expr -> Code
comp (Val n  ) = [PUSH n]
comp (Add l r) = comp l ++ comp r ++ [ADD]

-- 基底部: 
-- comp' (Var n) c
-- = {comp'の定義}
-- comp (Var n) ++ c
-- = {compを適用}
-- [PUSH n] ++ c
-- = {++を適用}
-- PUSH n : c
--
-- 再帰部:
-- comp' (Add l r)
-- = {comp'の定義}
-- comp (Add l r) ++ c
-- = {compを適用}
-- comp l ++ comp r ++ [ADD] ++ c
-- = {最後の++を適用}
-- comp l ++ comp r ++ (ADD : c)
-- = {++の結合の性質から}
-- comp l ++ (comp r ++ (ADD : c))
-- = {仮定より}
-- comp l ++ (comp' r (ADD : c))
-- = {仮定より}
-- comp' l (comp' r (ADD : c))
--
