module Sample where

data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr
          deriving Show

e1 :: Expr
e1 = Add (Add (Val 1) (Val 2)) (Val 3)

e2 :: Expr
e2 = Add (Add (Add (Val 1) (Val 2)) (Val 3)) Throw

e3 :: Expr
e3 = Catch (Add (Val 1) (Val 2)) (Val 99)

e4 :: Expr
e4 = Catch (Add Throw (Val 2)) (Val 99)

eval :: Expr -> Maybe Int
eval (Val n  )   = Just n
eval (Add x y)   = (+) <$> eval x <*> eval y
eval Throw       = Nothing
eval (Catch x h) = case eval x of
  Just n  -> Just n
  Nothing -> eval h

type Stack = [Int]

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : xs) = (n + m) : xs
add _            = error "invalid add operation"

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

