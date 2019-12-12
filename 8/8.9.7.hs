module Chap897 where

class Eq' a where
  (===), (/=) :: a -> a -> Bool
  x /= y = not (x === y)

instance Eq' Integer where
  x === y = x == y

instance Eq' a => Eq' (Maybe a) where
  Nothing === Nothing = True
  Nothing === _       = False
  _       === Nothing = False
  Just x  === Just y  = x === y

instance Eq' a => Eq' [a] where
  []       === []       = True
  []       === _        = False
  _        === []       = False
  (x : xs) === (y : ys) = y === x && xs === ys


