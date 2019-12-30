module Sample where

inc :: [Int] -> [Int]
inc []       = []
inc (x : xs) = x + 1 : inc xs

sqr :: [Int] -> [Int]
sqr []       = []
sqr (x : xs) = x * x : sqr xs

inc' :: [Int] -> [Int]
inc' = map (1 +)

sqr' :: [Int] -> [Int]
sqr' = map (^ (2 :: Int))

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x  ) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)


inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+ 1)
