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
--

-- 12.5.6
-- Monad
-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b

-- (r -> a)
-- ((->) r a)
-- pure :: a -> f a
--         a -> ((->) r a)
--         a -> (r -> a)
-- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
newtype MyFunc r a = F (r -> a)

applyF :: MyFunc r a -> r -> a
applyF (F f) = f


-- 12.5.3から
instance Functor (MyFunc r) where
  -- fmap :: (a -> b) -> MyFunc r a -> MyFunc r b
  -- fmap :: (a -> b) -> F (r -> a) -> F (r -> b)
  --         f              g
  fmap f (F g) = F (f . g)


instance Applicative (MyFunc r) where
  pure x = F (\_ -> x)
  -- (<*>) :: MyFunc r (a -> b) -> MyFunc r a -> MyFunc r b
  -- (<*>) :: F (r -> (a -> b)) -> F (r -> a) -> F (r -> b)
  F f <*> F g = F (\x -> f x (g x))


instance Monad (MyFunc r) where
  -- return :: a -> m a
  return x = F (\_ -> x)
  -- (>>=) :: MyFunc r a -> (a -> MyFunc r b) -> MyFunc r b
  -- (>>=) :: F (r -> a) -> (a -> F (r -> b)) -> F (r -> b)
  F g >>= h = F (\x -> applyF (h (g x)) x)

-- 12.5.7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
            deriving Show

expr1 :: Expr String
expr1 = Add (Add (Var "var1") (Val 2)) (Add (Val 3) (Var "var2"))

eval :: Expr Int -> Int
eval (Var x  ) = x
eval (Val x  ) = x
eval (Add l r) = eval l + eval r

dict :: String -> Int
dict "var1" = 10
dict "var2" = 20
dict _      = 0

dictM :: String -> Expr Int
dictM "var1" = Var 10
dictM "var2" = Var 20
dictM _      = Var 0

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a  ) = Var (f a)
  fmap f (Add l r) = Add (fmap f l) (fmap f r)
  fmap _ (Val x  ) = Val x

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- fmap :: Expr (a -> b) -> Expr a -> Expr b
  (Var f) <*> (Var x  ) = Var (f x)
  (Var _) <*> (Val x  ) = Val x
  (Var f) <*> (Add l r) = Add (fmap f l) (fmap f r)

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var x  ) >>= f = f x
  (Add l r) >>= f = Add (l >>= f) (r >>= f)
  (Val x  ) >>= _ = Val x

-- 12.5.8
-- まずオリジナルのSTの定義

type State = [Int]
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S f) = f

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f (S g) = S (\s -> let (x, s') = g s in (f x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  (S ff) <*> (S gg) = S
    (\s ->
      let (f, s' ) = ff s
          (x, s'') = gg s'
      in  (f x, s'')
    )

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  (S x) >>= f = S (\s -> let (v, s') = x s in app (f v) s')

push :: Int -> ST ()
push x = S (\xs -> ((), x : xs))

pop :: ST Int
pop = S (\(x : xs) -> (x, xs))

-- app stackTest [1, 2, 3, 4]
-- (2,[1,1,3,4])
stackTest :: ST Int
stackTest = do
  a <- pop
  _ <- pop
  push 1
  push a
  push 2
  pop


-- Monadの定義からApplicative, Functorを導出
newtype ST' a = S' (State -> (a, State))

app' :: ST' a -> State -> (a, State)
app' (S' f) = f

instance Monad ST' where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  (S' x) >>= f = S' (\s -> let (v, s') = x s in app' (f v) s')

instance Functor ST' where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = do
    x <- st
    return (f x)

instance Applicative ST' where
  -- pure :: a -> ST a
  pure x = S' (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    return (f x)

push' :: Int -> ST' ()
push' x = S' (\xs -> ((), x : xs))

pop' :: ST' Int
pop' = S' (\(x : xs) -> (x, xs))

-- Functor の確認
-- let st = pop'
-- let st' = fmap (1+) st
-- app' st' [1, 2, 3, 4]
-- => (2,[2,3,4])

-- Applicative の確認
-- let st'' = pure (\x -> x * x) <*>  st'
-- app' st'' [1, 2, 3, 4]
-- => (4,[2,3,4])
