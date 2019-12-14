module Sample where

import           Data.List

data Op = Add | Sub | Mul | Div | Pow
        deriving (Eq, Ord)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

-- valid :: Op -> Integer -> Integer -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

valid :: Op -> Integer -> Integer -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && x `mod` y == 0
valid Pow _ _ = True

-- 9.11.5 のvalid
-- length [e | ns' <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns', not $ null $ eval e]
-- 10839369
-- になる
-- valid :: Op -> Integer -> Integer -> Bool
-- valid Add _ _ = True
-- valid Sub x y = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y

data Expr = Val Integer | App Op Expr Expr

instance Eq Expr where
  (Val x        ) == (Val y        ) = x == y
  (App op1 x1 x2) == (App op2 y1 y2) = op1 == op2 && x1 == y1 && x2 == y2
  _               == _               = False

instance Ord Expr where
  compare (Val x) (Val y) = compare x y
  compare (Val _) _       = LT
  compare (App op1 x1 x2) (App op2 y1 y2)
    | op1 == op2 && x1 == x2 = compare x2 y2
    | op1 == op2             = compare x1 y1
    | otherwise              = compare op1 op2
  compare App{} _ = GT


instance Show Expr where
  show (Val n    ) = show n
  show (App o l r) = brak l ++ show o ++ brak r
   where
    brak (Val e) = show e
    brak e       = "(" ++ show e ++ ")"

e1 :: Expr
e1 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

values :: Expr -> [Integer]
values (Val e    ) = [e]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Integer]
eval (Val e    ) = [ e | e > 0 ]
eval (App o l r) = [ apply o x y | x <- eval l, y <- eval r, valid o x y ]

subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concatMap (interleave x) (perms xs)

choices :: [a] -> [[a]]
choices = concatMap perms . subs

-- 9.11.1
choices' :: [a] -> [[a]]
choices' ns = [ ps | sub <- subs ns, ps <- perms sub ]

solution :: Expr -> [Integer] -> Integer -> Bool
solution e xs n = elem (values e) (choices xs) && eval e == [n]

split :: [a] -> [([a], [a])]
split []       = []
split [_     ] = []
split (x : xs) = ([x], xs) : [ (x : ls, rs) | (ls, rs) <- split xs ]

split' :: [a] -> [([a], [a])]
split' []       = []
split' [x     ] = [([x], [])]
split' (x : xs) = ([x], xs) : [ (x : ls, rs) | (ls, rs) <- split' xs ]

combine :: Expr -> Expr -> [Expr]
combine l r = [ App o l r | o <- ops ]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Pow]

exprs :: [Integer] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r ]

solutions :: [Integer] -> Integer -> [Expr]
solutions ns n = [ e | ns' <- choices ns, e <- exprs ns', eval e == [n] ]


-- 9.11.4
-- 可能な式の確認
-- length [e | ns' <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns']
-- 33665406
--
-- 有効な式の確認
-- length [e | ns' <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns', not $ null $ eval e]
-- でいけるとおもったけど、366922 になってしまった。
-- とおもったけど、章中ででてきた改良validが使われてるせいだった、旧validを使うとちゃんと 4672540に。

type Result = (Expr, Integer)

results :: [Integer] -> [Result]
results []  = []
results [n] = [ (Val n, n) | n > 0 ]
results ns =
  [ e
  | (ls, rs) <- split ns
  , lx       <- results ls
  , rx       <- results rs
  , e        <- combine' lx rx
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [ (App o l r, apply o x y) | o <- ops, valid o x y ]

solutions' :: [Integer] -> Integer -> [Expr]
-- solutions' ns n = [ e | ns' <- choices ns, (e, m) <- results ns', m == n ]
solutions' ns n | null answers = mostNearestResult rs n
                | otherwise    = answers
 where
  rs      = [ e | ns' <- choices ns, e <- results ns' ]
  answers = [ e | (e, m) <- rs, m == n ]

-- mostNearestResult rs n = take 1 (sortBy (\(r1,v1) (r2,v2) -> compare (abs (n - v1)) (abs (n - v2))) rs)
mostNearestResult :: [Result] -> Integer -> [Expr]
mostNearestResult rs n =
  [ e
  | (e, _) <- take 1
    $ sortBy (\(_, v1) (_, v2) -> compare (abs (n - v1)) (abs (n - v2))) rs
  ]

-- 9.11.2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _        = True
isChoice _  []       = False
isChoice xs (y : ys) = isChoice (dropn y xs) ys

dropn :: Eq a => a -> [a] -> [a]
dropn _ [] = []
dropn x (y : ys) | x == y    = dropn x ys
                 | otherwise = y : dropn x ys

data Hoge = A | B
          deriving Show

instance Eq Hoge where
  A == A = True
  B == B = True
  _ == _ = False

instance Ord Hoge where
  compare A B = LT
  compare B A = GT
  compare _ _ = EQ

