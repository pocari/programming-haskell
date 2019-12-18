module Proactice where

import           Data.Char

-- 10.10.1
putStr' :: String -> IO ()
putStr' xs = sequence_ $ map putChar xs

-- 10.10.2
-- sample.hsで実装

-- 10.10.3
-- sample.hsで実装

-- 10.10.4
newline :: IO ()
newline = putChar '\n'

adder :: IO ()
adder = do
  putStr "How many numbers?: "
  x <- getChar
  newline
  adderHelper 0 (digitToInt x)
 where
  adderHelper s 0 = putStrLn $ "The total is " ++ show s
  adderHelper s r = do
    xs <- getLine
    adderHelper (s + (read xs :: Int)) (r - 1)

strs' :: IO [String]
strs' = do
  putStr "How many numbers?: "
  x <- getChar
  newline
  sequence $ replicate (digitToInt x) getLine

adder' :: IO ()
adder' = do
  xs <- strs'
  putStrLn $ "The total is " ++ show (sum $ map (\x -> read x :: Int) xs)



