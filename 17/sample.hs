module Sample where

data Expr = Val Int | Add Expr Expr
          deriving Show

e1 :: Expr
e1 = Add (Add (Val 1) (Val 2)) (Val 3)

type Stack = [Int]

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : xs) = (n + m) : xs
add _            = error "invalid add operation"

eval' :: Expr -> Stack -> Stack
eval' (Val n  ) s = push n s
eval' (Add l r) s = add (eval' l (eval' r s))

eval :: Expr -> Int
eval e = head $ eval' e []

