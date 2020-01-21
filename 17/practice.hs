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
eval' e = eval'' e haltC

type Cont = Stack -> Stack

eval'' :: Expr -> Cont -> Cont
-- eval'' e c = c (eval' e s)
-- の仕様を満たす
-- eval'' :: Expr -> Cont -> Cont
-- を探す
-- 
-- 基底部: Val
-- eval'' (Val n) c
-- = {eval''の仕様}
-- c (eval' (Val n) s)
-- = {eval' を適用}
-- c (push n s)
eval'' (Val n)     c s = pushC n c s

-- 基底部: Throw
-- eval'' Throw c
-- = {eval''の仕様}
-- c (eval' Throw s)
-- = {eval' を適用}
-- c (throw s)
eval'' Throw       c s = throwC c s

-- 再帰部: Add
-- eval'' (Add x y) c
-- = {eval''の仕様}
-- c (eval' (Add x y) s)
-- = {eval' を適用}
-- c (add (eval' y (eval' x s)))
-- = {.を逆適用}
-- (c . add) (eval' y (eval' x s))
-- = {yに対する仮定を適用}
-- eval'' y (c .add) (eval' x s)
-- = {xに対する仮定を適用}
-- eval'' x (eval'' y (c . add)) s
eval'' (Add   x y) c s = eval'' x (eval'' y (addC c)) s

-- 再帰部: Catch
-- eval'' (Catch x y) c
-- = {eval''の仕様}
-- c (eval' (Uatch x y) s)
-- = {eval' を適用}
-- c (catch (eval' y (eval' x s)))
-- = {.を逆適用}
-- (c . catch) (eval' y (eval' x s))
-- = {yに対する仮定を適用}
-- eval'' y (c .catch) (eval' x s)
-- = {xに対する仮定を適用}
-- eval'' y (eval'' x (c . catch)) s
eval'' (Catch x y) c s = eval'' y (eval'' x (catchC c)) s

-- comp :: Expr -> Code
-- comp e = comp' e HALT
-- 
-- comp' :: Expr -> Code -> Code
-- comp' (Val n  ) c = PUSH n c
-- comp' (Add x y) c = comp' x (comp' y (ADD c))
-- 
haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c s = c (push n s)

throwC :: Cont -> Cont
throwC c = c . throw

addC :: Cont -> Cont
addC c = c . add

catchC :: Cont -> Cont
catchC c = c . catch

-- data Code = HALT | PUSH Int Code | ADD Code
--           deriving Show
-- 
-- exec :: Code -> Stack -> Stack
-- exec HALT       s           = s
-- exec (PUSH n c) s           = exec c (n : s)
-- exec (ADD c   ) (m : n : s) = exec c (n + m : s)
-- exec _          _           = error "invalid pattern"

