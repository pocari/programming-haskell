module Chap8AbstraceMachine where

data Expr = Val Int | Add Expr Expr
          deriving Show

data Op = EVAL Expr | ADD Int
        deriving Show

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n  ) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)


exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD  n : c) m = exec c (n + m)

-- eval (Add (Val 1) (Val 2)) []
-- eval (Val 1) [(EVAL (Val 2))]
-- exec [(EVAL (Val 2))] 1
-- eval (Val 2) [(Add 1)]
-- exec [(Add 1)] 2
-- exec [] (1 + 2)
-- exec [] 3

value :: Expr -> Int
value e = eval e []

