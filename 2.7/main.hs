module Mian where

greet :: IO ()
greet = do
  putStr "hello"
  putStrLn " ,world"
  print n

n :: Int
n = a `div` length xs
 where
  a  = 10
  xs = [1, 2, 3, 4, 5 :: Int]


mylast1 :: [x] -> x
mylast1 xs = xs !! (length xs - 1)

mylast2 :: [x] -> x
mylast2 xs = head $ reverse xs

mylast3 :: [x] -> x
mylast3 xs = last xs

