module Chap893 where

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

--             .
--          /     \
--        .         .
--       / \       / \
--      1   2     3   4
--
t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

--             .
--          /     \
--        .         3
--       / \
--      1   2
--
t2 :: Tree Int
t2 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

--             .
--           /   \
--          1     .
--               / \
--              2   3
--
t3 :: Tree Int
t3 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

--             .
--           /   \
--          1     .
--               / \
--              .   4
--             / \
--            2   3
--
t4 :: Tree Int
t4 = Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4))

countLeaf :: Tree a -> Int
countLeaf (Leaf _  ) = 1
countLeaf (Node l r) = countLeaf l + countLeaf r

balanced :: Tree a -> Bool
balanced (Leaf _  ) = True
balanced (Node x y) = abs (countLeaf x - countLeaf y) <= 1

balance :: [a] -> Tree a
balance []  = error "hoge"
balance [x] = Leaf x
balance xs  = Node (balance x) (balance y)
  where (x, y) = splitAt (length xs `div` 2) xs

