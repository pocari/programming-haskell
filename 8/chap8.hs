module Chap8 where

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

type Pos = (Int, Int)
type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k x = head [ v | (k', v) <- x, k == k' ]

data Move = North | South | East | West
          deriving Show

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves []       p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

data Shape = Circle Float | Rect Float Float
           deriving Show

area :: Shape -> Float
area (Circle r  ) = pi * r * r
area (Rect r1 r2) = r1 * r2

data Nat = Zero | Succ Nat
         deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

data List a = Nil | Cons a (List a)
            deriving Show

len :: List a -> Int
len Nil        = 0
len (Cons _ x) = 1 + len x

data Tree a = Leaf a | Node (Tree a) a (Tree a)
            deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y    ) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x    ) = [x]
flatten (Node l y r) = flatten l ++ [y] ++ flatten r

-----------------------------------------------------------------------------
-- tautology checker

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving Show

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b  ) = b
eval s (Var   c  ) = find c s
eval s (Not   p  ) = not (eval s p)
eval s (And   x y) = eval s x && eval s y
eval s (Imply x y) = eval s x <= eval s y

vars :: Prop -> [Char]
vars (Const _  ) = []
vars (Var   c  ) = [c]
vars (Not   p  ) = vars p
vars (And   x y) = vars x ++ vars y
vars (Imply x y) = vars x ++ vars y

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p ]

