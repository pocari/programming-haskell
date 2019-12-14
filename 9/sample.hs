module Sample where

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n    ) = show n
  show (App o l r) = brak l ++ show o ++ brak r
   where
    brak (Val e) = show e
    brak e       = "(" ++ show e ++ ")"

e1 :: Expr
e1 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

values :: Expr -> [Int]
values (Val e    ) = [e]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val e    ) = [ e | e > 0 ]
eval (App o l r) = [ apply o x y | x <- eval l, y <- eval r, valid o x y ]

subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concatMap (interleave x) (perms xs)

choices :: [a] -> [[a]]
choices = concatMap perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e xs n = elem (values e) (choices xs) && eval e == [n]

