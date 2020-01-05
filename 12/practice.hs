module Practice where

data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

-- 12.5.1
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 12.5.2
-- すでにfunctorになっててduplicateエラーになったのでコメント
-- instance Functor ((->) a) where
--   -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- Applicative
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- (r -> a)
-- ((->) r a)
-- pure :: a -> f a
--         a -> ((->) r a)
--         a -> (r -> a)
-- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
-- 12.5.3
-- instance Applicative ((->) a) where
--   pure x = \_ -> x
--   f <*> g = \x -> f x (g x)

--  12.5.4
newtype MyZipList a = Z [a]
                    deriving Show

instance Functor MyZipList where
  -- fmap :: (a -> b) -> MyZipList a -> MyZipList b
  fmap f (Z xs) = Z (fmap f xs)

instance Applicative MyZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z (zipWith (\f x -> f x) gs xs)

