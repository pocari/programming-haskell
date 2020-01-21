module Sample where

data Expr = Val Int | Add Expr Expr
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
eval e = head $ exec (comp e) []

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
-- comp' e c = c (comp e s)
-- の仕様を満たす
-- comp' :: Expr -> Code -> Code
-- を探す
-- 
-- 基底部: Val
-- comp' (Val n) c
-- = {comp'の仕様}
-- c (comp (Val n) s)
-- = {comp を適用}
-- c (push n s)
comp' (Val n)     c = PUSH n c

-- 基底部: Throw
-- comp' Throw c
-- = {comp'の仕様}
-- c (comp Throw s)
-- = {comp を適用}
-- c (throw s)
comp' Throw       c = THROW c

-- 再帰部: Add
-- comp' (Add x y) c
-- = {comp'の仕様}
-- c (comp (Add x y) s)
-- = {comp を適用}
-- c (add (comp y (comp x s)))
-- = {.を逆適用}
-- (c . add) (comp y (comp x s))
-- = {yに対する仮定を適用}
-- comp' y (c .add) (comp x s)
-- = {xに対する仮定を適用}
-- comp' x (comp' y (c . add)) s
comp' (Add   x y) c = comp' x (comp' y (ADD c))

-- 再帰部: Catch
-- comp' (Catch x y) c
-- = {comp'の仕様}
-- c (comp (Uatch x y) s)
-- = {comp を適用}
-- c (catch (comp y (comp x s)))
-- = {.を逆適用}
-- (c . catch) (comp y (comp x s))
-- = {yに対する仮定を適用}
-- comp' y (c .catch) (comp x s)
-- = {xに対する仮定を適用}
-- comp' y (comp' x (c . catch)) s
comp' (Catch x y) c = comp' y (comp' x (CATCH c))

data Code = HALT
          | PUSH Int Code
          | ADD Code
          | THROW Code
          | CATCH Code
          deriving Show

exec :: Code -> Stack -> Stack
-- exec HALT       s = haltC s
-- haltC s
-- = {haltCを適用}
-- id s
-- = {idを適用}
-- s
-- exec HALT = s
exec HALT       s            = s

-- exec (PUSH n c) s = pushC n (exec c) s
-- = {pushCを適用}
-- (exec c . push n) s
-- = {.を適用}
-- exec c (push n s)
-- = {pushを適用}
-- exec c (n : s)
exec (PUSH n c) s            = exec c (Just n : s)

-- exec (ADD   c) s = addC (exec c) s
-- = {addCを適用}
-- (exec c . add) s
-- = {.を適用}
-- exec c (add s)
-- = {s を m : n : s' とおく}
-- exec c (add (m : n : s'))
-- = {add を適用}
-- exec c (n + m : s')
exec (ADD   c ) (m : n : s)  = exec c (((+) <$> n <*> m) : s)
exec (ADD   _ ) _            = error "invalid ADD pattern"

-- exec (THROW c) s = throwC (exec c) s
-- = {throwCを適用}
-- (exec c . throw) s
-- = {.を適用}
-- exec c (throw s)
-- = {throwを適用}
-- exec c (Nothing : s)
exec (THROW c ) s            = exec c (Nothing : s)

-- exec (CATCH c) s = catchC (exec c) s
-- = {catchCを適用}
-- (exec c . catch) s
-- = {.を適用}
-- exec c (catch s)
-- = {s を m : n : s' とおく}
-- exec c (catch (m : n : s'))
-- = {catchを適用}
-- exec c (case m of
--   Just n  -> Just x : xs
--   Nothing -> n : xs
-- )
exec (CATCH c ) (m : n : xs) = exec
  c
  (case m of
    Just x  -> Just x : xs
    Nothing -> n : xs
  )
exec (CATCH _) _ = error "invalid CATCH pattern"

