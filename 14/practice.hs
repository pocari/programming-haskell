{-# OPTIONS -Wall -Werror #-}
module Practice where
import           Data.Foldable
import           Data.Traversable               ( )
newtype MyMonoid a b = MM (a, b)
                     deriving Show

-- GHC 8.4以上は Semigroup が Monoidのスーパークラスになったそうで、
-- Monoidとして定義するために、は MyMonoid が Semigroup でもある必要があるらしい
-- ざっくりいうと、memtpyがない二項演算ができるだけ、というclassらし
instance (Monoid a, Monoid b) => Semigroup (MyMonoid a b) where
  -- (<>) :: MyMonoid a b -> MyMonoid a b -> MyMonoid a b
  MM (a, b) <> MM (x, y) = MM (a <> x, b <> y)

-- 14.5.1
instance (Monoid a, Monoid b) => Monoid (MyMonoid a b) where
  -- mempty :: MyMonoid a b
  mempty  = MM (mempty, mempty)

  -- maappend :: MyMonoid a b -> MyMonoid a b -> MyMonoid a b
  mappend = (<>)


-- 14.5.2
newtype MyFunc a b = F (a -> b)

instance Monoid b => Semigroup (MyFunc a b) where
  -- (<>) :: MyFunc a b -> MyFunc a b -> MyFunc a b
  F f <> F g = F (\x -> f x <> g x)

instance (Monoid b) => Monoid (MyFunc a b) where
  -- mempty :: MyFunc a b
  mempty  = F (\_ -> mempty)

  -- maappend :: MyMonoid a b -> MyMonoid a b -> MyMonoid a b
  mappend = (<>)

-- 14.5.3
newtype MyMaybe a = MyMaybe (Maybe a)
                  deriving Show

instance Foldable MyMaybe where
  -- fold :: Monoid a => MyMaybe a -> a
  fold (MyMaybe Nothing ) = mempty
  fold (MyMaybe (Just x)) = x

  -- foldMap :: Monoid b => (a -> b) -> MyMaybe a -> b
  foldMap _ (MyMaybe Nothing ) = mempty
  foldMap f (MyMaybe (Just x)) = f x

  -- foldr :: (a -> b -> b) -> b -> MyMaybe a -> b
  foldr _ x (MyMaybe Nothing ) = x
  foldr f x (MyMaybe (Just y)) = f y x

  -- foldl :: (a -> b -> a) -> a -> MyMaybe b -> a
  foldl _ x (MyMaybe Nothing ) = x
  foldl f x (MyMaybe (Just y)) = f x y

-- Traversable にするには、 MyMaybeがfunctorでもないといけないので定義
instance Functor MyMaybe where
  -- fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ (MyMaybe Nothing ) = MyMaybe Nothing
  fmap f (MyMaybe (Just x)) = MyMaybe (Just (f x))

instance Traversable MyMaybe where
  -- traverse :: Applicative f => (a -> f b) -> MyMaybe a -> f (MyMabe b)
  traverse _ (MyMaybe Nothing ) = pure (MyMaybe Nothing)
  traverse g (MyMaybe (Just x)) = fmap (MyMaybe . Just) (g x)

-- 14.5.4
data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf         = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r

-- test
dec :: Int -> Maybe Int
dec 0 = Nothing
dec x = Just (x - 1)

inc :: Int -> Maybe Int
inc 9 = Nothing
inc x = Just (x + 1)

t1 :: Tree Int
t1 = Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)
--            3
--           / \
--          /   \
--         /     \
--        1       4
--       / \     / \
--      /   \   L   L
--     /     \
--    0       2
--   / \     / \
--  /   \   /   \
-- L     L L     L

-- 14.5.5
myFilterF :: Foldable t => (a -> Bool) -> t a -> [a]
-- myFilterF f x = foldMap (\y -> if f y then [y] else []) x
myFilterF f = foldMap (\y -> [ y | f y ])

