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

-- 12.5.5
-- アプリカティブ則
-- pure id <*> x   = x
-- pure (g x)      = pure g <*> pure x
-- x <*> pure y    = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
--
-- Maybe Int でチェック
-- pure id <*> x   = x
-- 左辺
--  id :: Int -> Int
--  x  :: Myabe Int
-- 右辺
--  x  :: Mybe Int

-- pure (g x)      = pure g <*> pure x
-- 左辺
--  g :: Int -> a
--  x :: Int
-- 右辺
--  g :: Int -> a
--  x :: Int

-- x <*> pure y    = pure (\g -> g y) <*> x
-- 左辺
--  x :: Int -> a
--  y :: Int
-- 右辺
--  g :: Int -> a
--  x :: Maybe Int

-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- 左辺
-- x :: Maby (a -> b)
-- y :: Maby (Int -> a)
-- z : Maybe Int
-- 右辺
