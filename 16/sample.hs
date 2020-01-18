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


