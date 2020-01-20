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

eval :: Expr -> Int
eval e = head $ exec (comp e) []

type Cont = Stack -> Stack

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n  ) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c s = c (push n s)

addC :: Cont -> Cont
addC c = c . add

data Code = HALT | PUSH Int Code | ADD Code
          deriving Show

exec :: Code -> Stack -> Stack
exec HALT       s           = s
exec (PUSH n c) s           = exec c (n : s)
exec (ADD c   ) (m : n : s) = exec c (n + m : s)
exec _          _           = error "invalid pattern"

