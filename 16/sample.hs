module Sample where

reverse1 :: [a] -> [a]
reverse1 []       = []
reverse1 (x : xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 xs = reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' []       ys = ys
reverse' (x : xs) ys = reverse' xs (x : ys)

data Tree a = Leaf a | Node (Tree a) (Tree a)
          deriving Show

t1 :: Tree Int
t1 = Node (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) (Leaf 5)

flatten1 :: Tree a -> [a]
flatten1 (Leaf x  ) = [x]
flatten1 (Node l r) = flatten1 l ++ flatten1 r

flatten2 :: Tree a -> [a]
flatten2 t = flatten' t []

flatten' :: Tree a -> [a] -> [a]
flatten' (Leaf x  ) ys = x : ys
flatten' (Node l r) ys = flatten' l (flatten' r ys)

