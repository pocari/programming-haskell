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

type Stack = [Maybe Int]

push :: Int -> Stack -> Stack
push n s = Just n : s

add :: Stack -> Stack
add (m : n : xs) = ((+) <$> n <*> m) : xs
add _            = error "invalid add operation"

throw :: Stack -> Stack
throw s = Nothing : s

catch :: Stack -> Stack
catch (m : n : xs) = case m of
  Just x  -> Just x : xs
  Nothing -> n : xs
catch _ = error "invalid catch operation"

eval :: Expr -> Maybe Int
eval e = head $ eval' e []

eval' :: Expr -> Stack -> Stack
-- eval' e s = eval e : s
-- の仕様を満たす
-- eval' :: Expr -> Stack -> Stack
-- を探す
--
-- 基底部: Val
-- eval' (Val n) s
-- = {eval'の仕様}
-- eval (Val n) : s
-- = {evalを適用}
-- Just n : s
-- = {pushを逆適用}
-- push n s
eval' (Val n)     s = push n s
--
-- 基底部: Throw
-- eval' Throw s
-- = {evalの仕様}
-- eval Throw : s
-- = {evalを適用}
-- Nothing : s
-- = {throwを逆適用}
-- throw s
eval' Throw       s = throw s
--
-- 再帰部(Add):
-- eval' (Add x y) s
-- = {eval'の仕様}
-- eval (Add x y) x
-- = {evalを適用}
-- ((+) <$> eval x <*> eval y) : s
-- = {addを逆適用}
-- add (eval x : eval y : s)
-- = {xに対する仮定}
-- add (eval x (eval y : s))
-- = {yに対する仮定}
-- add (eval' x (eval' y s))
eval' (Add   x y) s = add (eval' x (eval' y s))
--
-- 再帰部(Catch):
-- eval' (Catch x y) s
-- = {eval'の仕様}
-- eval (Catch x y) : s
-- = {evalを適用}
-- let e = case eval x of
--   Just n  -> Just n
--   Nothing -> eval h
-- in e : s
-- = {catchを逆適用}
-- catch (eval x : eval y : xs)
-- = {xに対する仮定}
-- catch (eval x (eval y : s))
-- = {yに対する仮定}
-- catch (eval' x (eval' y s))
eval' (Catch x y) s = catch (eval' x (eval' y s))

-- type Cont = Stack -> Stack

-- comp :: Expr -> Code
-- comp e = comp' e HALT
-- 
-- comp' :: Expr -> Code -> Code
-- comp' (Val n  ) c = PUSH n c
-- comp' (Add x y) c = comp' x (comp' y (ADD c))
-- 
-- haltC :: Cont
-- haltC = id
-- 
-- pushC :: Int -> Cont -> Cont
-- pushC n c s = c (push n s)
-- 
-- addC :: Cont -> Cont
-- addC c = c . add
-- 
-- data Code = HALT | PUSH Int Code | ADD Code
--           deriving Show
-- 
-- exec :: Code -> Stack -> Stack
-- exec HALT       s           = s
-- exec (PUSH n c) s           = exec c (n : s)
-- exec (ADD c   ) (m : n : s) = exec c (n + m : s)
-- exec _          _           = error "invalid pattern"

