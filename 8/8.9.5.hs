module Chap895 where

data Expr = Val Int | Add Expr Expr
          deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x  ) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)


e1 :: Expr
e1 = Add (Add (Val 1) (Val 2)) (Val 3)

e2 :: Expr
e2 = Val 2

e3 :: Expr
e3 = Add (Val 2) (Add (Val 3) (Val 4))

