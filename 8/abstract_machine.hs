module Chap8AbstraceMachine where

data Expr = Val Int | Add Expr Expr | Mult Expr Expr
          deriving Show

data Op = ADD Expr | MULT Expr
        deriving Show

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n   ) c = exec c n
eval (Add  x y) c = eval x (ADD y : c)
eval (Mult x y) c = eval x (MULT y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (ADD  e : c) m = exec c (eval e c) + m
exec (MULT e : c) m = exec c (eval e c) * m

-- eval (Add (Val 1) (Val 2)) []
-- eval (Val 1) [(EVAL (Val 2))]
-- exec [(EVAL (Val 2))] 1
-- eval (Val 2) [(Add 1)]
-- exec [(Add 1)] 2
-- exec [] (1 + 2)
-- exec [] 3

value :: Expr -> Int
value e = eval e []

