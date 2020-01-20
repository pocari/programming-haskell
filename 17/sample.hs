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
eval e = head $ eval' e []

type Cont = Stack -> Stack

eval' :: Expr -> Cont
eval' e = eval'' e haltC

eval'' :: Expr -> Cont -> Cont
eval'' (Val n  ) c s = pushC n c s
eval'' (Add x y) c s = eval'' x (eval'' y (addC c)) s

haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c s = c (push n s)

addC :: Cont -> Cont
addC c = c . add

data Code = HALT | PUSH Int Code | ADD Code
          deriving Show

exec :: Code -> Cont
exec HALT       = haltC
exec (PUSH n c) = pushC n (exec c)
exec (ADD c   ) = addC (exec c)

